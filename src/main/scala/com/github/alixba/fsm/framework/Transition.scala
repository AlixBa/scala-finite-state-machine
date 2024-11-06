package com.github.alixba.fsm.framework

type Transition[S0, I, S1] = (S0, I) => S1
