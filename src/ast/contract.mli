type case_interface = Ethereum.function_signature

val case_interface_of : 'exp Syntax.case -> case_interface

type contract_api =
  { contract_api_name : string
    (** [contract_api_name] is the name of the contract. *)
  ; contract_api_args : Syntax.typ list
  ; contract_api_cases : case_interface list
  ; contract_api_continuations : string list
    (** [contract_api_continuations] lists the names of contracts that
        this one can continue into *)
  }

type interface_api = 
  { interface_api_name : string
    (** [contract_interface_api_name] is the name of the contract. *)
  ; interface_api_cases : case_interface list
  }

type api = 
  | ContractAPI of contract_api
  | InterfaceAPI of interface_api

val contract_api_of : 'exp Syntax.contract -> contract_api

val interface_api_of : Syntax.interface -> interface_api

val api_of : 'exp Syntax.toplevel -> api

val find_method_signature :
  api Assoc.contract_id_assoc ->
  string (* contract name *) -> string (* method name *) -> case_interface option
