%% @author Aldrin
%% @doc @todo Add description to maths_properties.


-module(maths_properties_workers).

%% ====================================================================
%% API functions
%% ====================================================================
-export([prop_always_neverdup_resp/1]).


prop_always_neverdup_resp(seq) ->
    
    NecRequest = seq_main_mon:m_nec(recv, 
                                   fun(_Actor)-> true end, 
                                   fun(Message) -> case Message of {request, _, _, _} -> true; _ -> false end end),
    
    NecResp = seq_main_mon:m_nec(send, 
                                 fun(_Actor)-> true end, 
                                 fun(Message) -> case Message of {result, _Result, _OriginalClient} -> true; _ -> false end end),
    
    Correct = {[NecRequest, NecResp, NecResp, seq_main_mon:m_fls()], 1, seq_main_mon:new_vars()},
    Recurse = {[NecRequest, NecResp, seq_main_mon:m_var(x)], 1, seq_main_mon:new_vars()},
    
    PropAnd = seq_main_mon:m_and(Correct, Recurse),
    
    {[seq_main_mon:m_max(x,PropAnd), PropAnd], 1, seq_main_mon:new_vars()};

prop_always_neverdup_resp(conc) ->
    fun([Server]) ->
        
        AllActorSpec = fun(_Actor)-> true end,
        ServerActorSpec = fun(Actor)-> case Actor of Server -> true; _ -> false end end, 
        ReqMessageSpec = fun(Message) -> case Message of {request, _, _, _} -> true; _ -> false end end,
        RespMessageSpec = fun(Message) -> case Message of {result,_, _} -> true; _ -> false end end,
        
        CorrectnessProp = detectEr:m_nec(recv, AllActorSpec, ReqMessageSpec, 
                                            detectEr:m_nec(send, AllActorSpec, RespMessageSpec, 
                                                           detectEr:m_nec(send, AllActorSpec, RespMessageSpec,
                                                                          detectEr:m_fls()))),
        
        RecursePropFact =  detectEr:m_nec(recv, AllActorSpec, ReqMessageSpec, 
                                          detectEr:m_nec(send, AllActorSpec, RespMessageSpec, 
                                                         detectEr:m_var(x))),
                                        
        detectEr:m_and(detectEr:m_trace(), 
                       detectEr:m_max(x, detectEr:m_and(CorrectnessProp, RecursePropFact)))
    end.