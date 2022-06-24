# 1 "./contracts/main.mligo"
# 1 "./contracts/./interface.mligo" 1

type token_id = nat
type ipfs_hash = bytes

(*
    TRANSFER PARAMS
*)

type transfer_to =
[@layout:comb]
{
    to_     : address;
    token_id: token_id;
    amount  : nat;
}

type transfer_param = 
[@layout:comb]
{
    from_   : address;
    txs     : transfer_to list
}

(*
    UPDATE OPERATORS PARAMS
*)

type operator =
[@layout:comb]
{
    owner   : address;
    operator: address;
    token_id: token_id
}

type update_operators_param =
| Add_operator of operator
| Remove_operator of operator

(*
    BALANCE OF PARAMS
*)

type balance_of_request =
[@layout:comb]
{
    owner   : address;
    token_id: token_id;
}

type balance_of_callback_param =
[@layout:comb]
{
    request: balance_of_request;
    balance: nat;
}

type balance_of_param =
[@layout:comb]
{
    requests: balance_of_request list;
    callback: (balance_of_callback_param list) contract;
}

(*
    STORAGE AND PARAMETER
*)

// parameter type
type parameter = 
    | Transfer of transfer_param list
    | Update_operators of update_operators_param list
    | Balance_of of balance_of_param
    | Mint
    | Update_admin of address
    | Update_metadata of bytes
    | Update_token_metadata of (token_id * bytes)
    | Set_pause

// storage type
type ledger = ((address * token_id), nat) big_map
type token_metadata = 
[@layout:comb]
{
    token_id: nat;
    token_info: (string, bytes) map
}
type storage =
{
    ledger              : ledger;
    operators           : (operator, unit) big_map;
    metadata            : (string, bytes) big_map;
    token_metadata      : (token_id, token_metadata) big_map;
    total_nfts          : nat;
    admin               : address;
    paused              : bool;
}

type return = (operation list) * storage
# 2 "./contracts/main.mligo" 2

# 1 "./contracts/./utils.mligo" 1
(*
    UTILS functions reserved for admin
*)

// checks if the current sender is the admin
let is_admin (user_address: address) (s: storage): unit =
    if user_address <> s.admin
    then (failwith "NOT_AN_ADMIN": unit)
    else ()

// checks if the contract is paused
let is_paused (s: storage): unit =
    if s.paused = true
    then (failwith "CONTRACT_PAUSED": unit)
    else ()

(* Updates the admin's address *)
let update_admin (p, s: address * storage): storage =
    let _ = is_admin Tezos.sender s in { s with admin = p }

(* Updates the metadata *)
let update_metadata (p, s: bytes * storage): storage =
    let _ = is_admin Tezos.sender s in { s with metadata = Big_map.update "contents" (Some (p)) s.metadata }

(* Updates the token metadata *)
let update_token_metadata (p, s: (token_id * bytes) * storage): storage =
    let _ = is_admin Tezos.sender s in
    let (token_id, metadata) = p in
    if not Big_map.mem token_id s.token_metadata
    then (failwith "FA2_TOKEN_UNDEFINED": storage)
    else
        let new_token_info = {
            token_id = token_id;
            token_info = Map.literal [ ("", metadata) ]
        } 
        in { s with token_metadata = Big_map.update token_id (Some new_token_info) s.token_metadata }

(* Mints additional tokens *)
let mint (s: storage): storage =
    let _ = is_paused s in
    // verifies that the sender hasn't already minted an NFT
    if Big_map.mem (Tezos.sender, 0n) s.ledger
    then (failwith "ALREADY_CLAIMED": storage)
    else
        // retrieves the last minted token metadata
        {
            s with 
                ledger      = Big_map.add (Tezos.sender, 0n) 1n s.ledger;
                total_nfts  = s.total_nfts + 1n;
        }

(* Pauses the contract *)
let set_pause (s: storage): storage =
    let _ = is_admin Tezos.sender s in { s with paused = not s.paused}
# 3 "./contracts/main.mligo" 2

# 1 "./contracts/./transfer.mligo" 1
let apply_transfer (((from, s), transfer): (address * storage) * transfer_to): address * storage =
    let { to_ = recipient; token_id = token_id; amount = amt } = transfer in
    // checks if token_id is valid
    if not Big_map.mem token_id s.token_metadata
    then (failwith "FA2_TOKEN_UNDEFINED": address * storage)
    else
        // checks is sender is allowed to request a transfer
        let operator = { owner = from; operator = Tezos.sender; token_id = token_id } in
        if Tezos.sender <> from && not Big_map.mem operator s.operators
        then (failwith "FA2_NOT_OPERATOR": address * storage)
        else
            // updates the sender's account
            let new_ledger: ledger = 
                match Big_map.find_opt (from, token_id) s.ledger with
                | None -> (failwith "FA2_INSUFFICIENT_BALANCE": ledger)
                | Some blnc ->
                    if blnc < amt
                    then (failwith "FA2_INSUFFICIENT_BALANCE": ledger)
                    else Big_map.update (from, token_id) (Some (abs (blnc - amt))) s.ledger
            in
            // adds the token to the recipient's account
            let new_ledger: ledger =
                match Big_map.find_opt (recipient, token_id) new_ledger with
                | None -> Big_map.add (recipient, token_id) amt new_ledger
                | Some blnc -> Big_map.update (recipient, token_id) (Some (blnc + amt)) new_ledger
            in

            from, { s with ledger = new_ledger }

let process_transfer (s, transfer: storage * transfer_param): storage =
    let { from_ = from; txs = txs } = transfer in
    let (_, new_storage): address * storage =
        List.fold apply_transfer txs (from, s)
    in new_storage

let transfer (transfer_list, s: (transfer_param list) * storage): storage =
    let _ = is_paused s in
    List.fold process_transfer transfer_list s
# 4 "./contracts/main.mligo" 2

# 1 "./contracts/./update_operators.mligo" 1
let update_operators (operators_list, s: (update_operators_param list) * storage): storage =
    let _ = is_paused s in
    List.fold
        (
            fun ((s, operator_param): storage * update_operators_param) ->
                match operator_param with
                | Add_operator operator ->
                    if Tezos.sender <> operator.owner
                    then (failwith "FA2_NOT_OWNER": storage)
                    else
                        { s with operators = Big_map.add operator unit s.operators }
                | Remove_operator operator->
                    if Tezos.sender <> operator.owner
                    then (failwith "FA2_NOT_OWNER": storage)
                    else
                        { s with operators = Big_map.remove operator s.operators }
        )
        operators_list
        s
# 5 "./contracts/main.mligo" 2

# 1 "./contracts/./balance_of.mligo" 1
let balance_of (p, s: balance_of_param * storage): operation list * storage =
    // creates the list of all requested balances
    let list_of_balances: balance_of_callback_param list =
        List.map
            (
                fun (req: balance_of_request): balance_of_callback_param ->
                    if not Big_map.mem req.token_id s.token_metadata
                    then (failwith "FA2_TOKEN_UNDEFINED": balance_of_callback_param)
                    else
                        match Big_map.find_opt (req.owner, req.token_id) s.ledger with
                        | None -> { request = { owner = req.owner; token_id = req.token_id }; balance = 0n }
                        | Some b -> { request = { owner = req.owner; token_id = req.token_id }; balance = b }
            )
            p.requests
    in
    // forges operation for callback and returns storage
    [Tezos.transaction list_of_balances 0tez p.callback], s
# 6 "./contracts/main.mligo" 2

let main (action, storage : parameter * storage) : return =
    if Tezos.amount <> 0tez
    then (failwith "NO_XTZ_AMOUNT": return)
    else
        match action with
            | Transfer p -> ([]: operation list), transfer (p, storage)
            | Update_operators p -> ([]: operation list), update_operators (p, storage)
            | Balance_of p -> balance_of (p, storage)
            | Mint -> ([]: operation list), mint storage
            | Update_admin p -> ([]: operation list), update_admin (p, storage)
            | Update_metadata p -> ([]: operation list), update_metadata (p, storage)
            | Update_token_metadata p -> ([]: operation list), update_token_metadata (p, storage)
            | Set_pause -> ([]: operation list), set_pause storage

[@view]
let balance_of ((owner, token_id), s: (address * token_id) * storage): nat option =
    Big_map.find_opt (owner, token_id) s.ledger

[@view]
let is_paused ((), s: unit * storage): bool = s.paused