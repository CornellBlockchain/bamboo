exception NoInterfaceFor of string (* for handling exhaustive pattern matching on line 110,
                                      not sure what to do there at the moment *)

type case_interface = Ethereum.function_signature

let case_interface_of_header (raw : Syntax.case_header) : case_interface = 
  match raw with 
  | Syntax.UsualCaseHeader header ->
    {Ethereum.sig_return = 
        List.map Ethereum.interpret_interface_type Syntax.(header.case_return_typ)
      ; sig_name = Syntax.(header.case_name)
      ; sig_args = List.map Ethereum.interpret_interface_type
                  Syntax.(List.map (fun x -> x.arg_typ) header.case_arguments)
    }
  | Syntax.DefaultCaseHeader ->
    { Ethereum.sig_return = []
      ; sig_name = "" (* is this a good choice? *)
      ; sig_args = []
    }

let case_interface_of (raw : 'exp Syntax.case) : case_interface =
  case_interface_of_header raw.case_header

type contract_api =
  { contract_api_name : string
    (** [contract_api_name] is the name of the contract. *)
  ; contract_api_args : Syntax.typ list
    (* Since [contract_api_args] contains bool[address] and such,
     * is's not appropriate to use the ABI signature here.
     * As a work around, at the time of deployment, these
     * arrays are zeroed out.
     *)
  ; contract_api_cases : case_interface list
  ; contract_api_continuations : string list
    (** [contract_api_continuations] lists the names of contracts that
        this one can continue into *)
  }

type interface_api = 
  { interface_api_name : string
    (** [interface_api_name] is the name of the contract. *)
  ; interface_api_cases : case_interface list
  }

type api = 
  | ContractAPI of contract_api
  | InterfaceAPI of interface_api

let rec collect_continuation_in_sentence (raw : 'exp Syntax.sentence) : string list =
  Syntax.(
    match raw with
    | AbortSentence -> []
    | ReturnSentence r ->
       begin
         match contract_name_of_return_cont r.return_cont with
         | None -> []
         | Some name -> [name]
       end
    | AssignmentSentence (_, _) -> []
    | VariableInitSentence _ -> []
    | SelfdestructSentence _ -> []
    | IfThenOnly (_, ss) ->
       collect_continuation_in_sentences ss
    | IfThenElse (_, s, t) ->
       (collect_continuation_in_sentences) s @ (collect_continuation_in_sentences t)
    | ExpSentence _ -> []
    | LogSentence _ -> []
  )
and collect_continuation_in_sentences ss =
  List.concat (List.map collect_continuation_in_sentence ss)

let collect_continuation_in_case (raw : 'exp Syntax.case) : string list =
  List.concat Syntax.(List.map collect_continuation_in_sentence raw.case_body)

let collect_continuation_in_contract (raw : 'exp Syntax.contract) : string list =
  List.concat Syntax.(List.map collect_continuation_in_case raw.contract_cases)

let contract_api_of (raw : 'exp Syntax.contract) : contract_api =
  Syntax.
  { contract_api_name = raw.contract_name
  ; contract_api_args = List.map (fun x -> x.arg_typ) raw.contract_arguments
  ; contract_api_cases = List.map case_interface_of raw.contract_cases
  ; contract_api_continuations = collect_continuation_in_contract raw
  }

let interface_api_of (raw : Syntax.interface) : interface_api =
  Syntax.
  { interface_api_name = raw.interface_name
  ; interface_api_cases = List.map case_interface_of_header raw.interface_cases
  }

let api_of (raw : 'exp Syntax.toplevel) : api =
  match raw with
  | Contract c -> ContractAPI (contract_api_of c)
  | Interface i -> InterfaceAPI (interface_api_of i)
  | Event e -> raise (NoInterfaceFor e.event_name)

let find_method_sig_in_contract
      (method_name : string) (i : api)
    : case_interface option =
  match i with
  | ContractAPI contract ->
    Misc.first_some (fun case_inter ->
        if case_inter.Ethereum.sig_name = method_name then
          Some case_inter
        else None
      ) contract.contract_api_cases
  | InterfaceAPI interface ->
    Misc.first_some (fun case_inter ->
        if case_inter.Ethereum.sig_name = method_name then
          Some case_inter
        else None
      ) interface.interface_api_cases

let find_method_signature
  (interfaces : api Assoc.contract_id_assoc)
  (contract_name : string)
  (method_name : string) : case_interface option =
  Misc.first_some (find_method_sig_in_contract method_name) (List.map snd interfaces)
