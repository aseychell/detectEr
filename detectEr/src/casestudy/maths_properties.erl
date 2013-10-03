%% @author Aldrin
%% @doc @todo Add description to maths_properties.


-module(maths_properties).

%% ====================================================================
%% API functions
%% ====================================================================
-export([prop_trace_seq/0,
         prop_correct_trace/0,
         prop_alwayscorrect_trace/2,
         prop_all_alwayscorrect_trace/2]).


prop_trace_seq() ->
    TracerMon = seq_main_mon:m_trace(),
    {[TracerMon], 1, seq_main_mon:new_vars()}.

prop_correct_trace() ->
    
    Property1 = {[seq_main_mon:m_trace()], 1, seq_main_mon:new_vars()},
    NecRequest = seq_main_mon:m_nec(recv, 
                                   fun(_Actor)-> true end, 
                                   fun(Message) -> case Message of {request,factorial, _,5} -> true; _ -> false end end),

    NecResp = seq_main_mon:m_nec(send, 
                                   fun(_Actor)-> true end, 
                                   fun(Message) -> case Message of {result,120} -> true; _ -> false end end),
    
    Fail = seq_main_mon:m_fls(),
    
    Property2 = {[NecRequest, NecResp, NecResp, Fail], 1, seq_main_mon:new_vars()},

%%     Property2.
    {[seq_main_mon:m_and(Property1, 
                         Property2)], 1, seq_main_mon:new_vars()}.


prop_alwayscorrect_trace(seq, Number) ->
    
    Property1 = {[seq_main_mon:m_trace()], 1, seq_main_mon:new_vars()},
    NecRequest = seq_main_mon:m_nec(recv, 
                                   fun(_Actor)-> true end, 
                                   fun(Message) -> case Message of {request,factorial, _,Number} -> true; _ -> false end end),

    NecResp = seq_main_mon:m_nec(send, 
                                   fun(_Actor)-> true end, 
                                   fun(Message) -> case Message of {result,_} -> true; _ -> false end end),
    
    Fail = seq_main_mon:m_fls(),
    
    PropUnderMaxFail = {[NecRequest, NecResp, NecResp, Fail], 1, seq_main_mon:new_vars()},
    PropUnderMaxRecurse = {[NecRequest, NecResp, NecRequest, seq_main_mon:m_var(x) ], 1, seq_main_mon:new_vars()},
    
    AndProp = seq_main_mon:m_and(PropUnderMaxFail, PropUnderMaxRecurse),
    
    Property2 = {[seq_main_mon:m_max(x, AndProp), AndProp], 1, seq_main_mon:new_vars()},
    {[seq_main_mon:m_and(Property1, Property2)], 1, seq_main_mon:new_vars()};
prop_alwayscorrect_trace(conc, Number) ->
    fun([Server]) ->
        CorrectnessProp = detectEr:m_nec(recv, 
                                         fun(Actor)-> case Actor of Server -> true; _ -> false end end, 
                                         fun(Message) -> case Message of {request,factorial, _,Number} -> true; _ -> false end end, 
                                         detectEr:m_nec(send, 
                                                        fun(_Actor)-> true end, 
                                                        fun(Message) -> case Message of {result,_} -> true; _ -> false end end, 
                                                        detectEr:m_nec(send, 
                                                                       fun(_Actor)-> true end, 
                                                                       fun(Message) -> case Message of {result,_} -> true; _ -> false end end,
                                                                       detectEr:m_fls()))),
            
        RecurseProp =  detectEr:m_nec(recv, 
                                      fun(Actor)-> case Actor of Server -> true; _ -> false end end, 
                                      fun(Message) -> case Message of {request,factorial, _,Number} -> true; _ -> false end end, 
                                      detectEr:m_nec(send, 
                                                     fun(_Actor)-> true end, 
                                                     fun(Message) -> case Message of {result, _} -> true; _ -> false end end, 
                                                     detectEr:m_nec(recv, 
                                                                    fun(Actor)-> case Actor of Server -> true; _ -> false end end, 
                                                                    fun(Message) -> case Message of {request,factorial, _, _} -> true; _ -> false end end,
                                                                    detectEr:m_var(x)))),
                                         
        detectEr:m_and(detectEr:m_trace(), 
                       detectEr:m_max(x, detectEr:m_and(CorrectnessProp, RecurseProp)))
    end.


prop_all_alwayscorrect_trace(seq, Number) ->
    
    NecRequestFact = seq_main_mon:m_nec(recv, 
                                   fun(_Actor)-> true end, 
                                   fun(Message) -> case Message of {request,factorial, _,Number} -> true; _ -> false end end),
    NecRequestSummation = seq_main_mon:m_nec(recv, 
                                   fun(_Actor)-> true end, 
                                   fun(Message) -> case Message of {request,summation, _,Number} -> true; _ -> false end end),
    NecRequestSumlist = seq_main_mon:m_nec(recv, 
                                   fun(_Actor)-> true end, 
                                   fun(Message) -> case Message of {request,sumlist, _, _List} -> true; _ -> false end end),

    NecResp = seq_main_mon:m_nec(send, 
                                   fun(_Actor)-> true end, 
                                   fun(Message) -> case Message of {result,_} -> true; _ -> false end end),
    
    CorrectFact = {[NecRequestFact, NecResp, NecResp, seq_main_mon:m_fls()], 1, seq_main_mon:new_vars()},
    CorrectSummation = {[NecRequestSummation, NecResp, NecResp, seq_main_mon:m_fls()], 1, seq_main_mon:new_vars()},
    CorrectSumlist = {[NecRequestSumlist, NecResp, NecResp, seq_main_mon:m_fls()], 1, seq_main_mon:new_vars()},
    
    CorrectSumAnd = {[seq_main_mon:m_and(CorrectSummation, CorrectSumlist)], 1, seq_main_mon:new_vars()},
    
    CorrectBranch = {[seq_main_mon:m_and(CorrectFact, CorrectSumAnd)], 1, seq_main_mon:new_vars()},
    
    RecurseFact = {[NecRequestFact, NecResp, seq_main_mon:m_var(x)], 1, seq_main_mon:new_vars()},
    RecurseSummation = {[NecRequestSummation, NecResp, seq_main_mon:m_var(x)], 1, seq_main_mon:new_vars()},
    RecurseSumlist = {[NecRequestSumlist, NecResp, seq_main_mon:m_var(x)], 1, seq_main_mon:new_vars()},
        
    RecurseSumAnd = {[seq_main_mon:m_and(RecurseSummation, RecurseSumlist)], 1, seq_main_mon:new_vars()},
    RecurseBranch = {[seq_main_mon:m_and(RecurseFact, RecurseSumAnd)], 1, seq_main_mon:new_vars()},
    
    PropAnd = seq_main_mon:m_and(CorrectBranch, RecurseBranch),
    
    {[seq_main_mon:m_max(x,PropAnd), PropAnd], 1, seq_main_mon:new_vars()};
prop_all_alwayscorrect_trace(conc, Number) ->
    fun([Server]) ->
        
        AllActorSpec = fun(_Actor)-> true end,
        ServerActorSpec = fun(Actor)-> case Actor of Server -> true; _ -> false end end, 
        RespMessageSpec = fun(Message) -> case Message of {result,_} -> true; _ -> false end end,
        
        CorrectnessPropFact = detectEr:m_nec(recv, ServerActorSpec,
                                            fun(Message) -> case Message of {request,factorial, _,Number} -> true; _ -> false end end, 
                                            detectEr:m_nec(send, AllActorSpec, RespMessageSpec, 
                                                           detectEr:m_nec(send, AllActorSpec, RespMessageSpec,
                                                                          detectEr:m_fls()))),
        CorrectnessPropSummation = detectEr:m_nec(recv, ServerActorSpec, 
                                                fun(Message) -> case Message of {request,summation, _,Number} -> true; _ -> false end end, 
                                                detectEr:m_nec(send, AllActorSpec, RespMessageSpec, 
                                                           detectEr:m_nec(send, AllActorSpec, RespMessageSpec,detectEr:m_fls()))),
        CorrectnessPropSummlist = detectEr:m_nec(recv, ServerActorSpec, 
                                                fun(Message) -> case Message of {request,sumlist, _,_List} -> true; _ -> false end end, 
                                                detectEr:m_nec(send, AllActorSpec, RespMessageSpec, 
                                                           detectEr:m_nec(send, AllActorSpec, RespMessageSpec, detectEr:m_fls()))),
            
        CorrectnessAndSums = detectEr:m_and(CorrectnessPropSummation, CorrectnessPropSummlist),
        CorrectnessBranch = detectEr:m_and(CorrectnessPropFact, CorrectnessAndSums),
        
        RecursePropFact =  detectEr:m_nec(recv, AllActorSpec, 
                                          fun(Message) -> case Message of {request,factorial, _,Number} -> true; _ -> false end end, 
                                          detectEr:m_nec(send, AllActorSpec, RespMessageSpec, detectEr:m_var(x))),
        RecursePropSummation =  detectEr:m_nec(recv, AllActorSpec, 
                                          fun(Message) -> case Message of {request,summation, _,Number} -> true; _ -> false end end, 
                                          detectEr:m_nec(send, AllActorSpec, RespMessageSpec, detectEr:m_var(x))),
        RecursePropSumlist =  detectEr:m_nec(recv, AllActorSpec, 
                                             fun(Message) -> case Message of {request,sumlist, _,Number} -> true; _ -> false end end, 
                                             detectEr:m_nec(send, AllActorSpec, RespMessageSpec, detectEr:m_var(x))),
        
        
        RecurseAndSums = detectEr:m_and(RecursePropSummation, RecursePropSumlist),
        RecurseBranch = detectEr:m_and(RecursePropFact, RecurseAndSums),
                                         
        detectEr:m_and(detectEr:m_trace(), 
                       detectEr:m_max(x, detectEr:m_and(CorrectnessBranch, RecurseBranch)))
    end.