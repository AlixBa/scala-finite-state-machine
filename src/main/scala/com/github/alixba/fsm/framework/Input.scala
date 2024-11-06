package com.github.alixba.fsm.framework

type Input[F[_], S, I] = S => F[I]
