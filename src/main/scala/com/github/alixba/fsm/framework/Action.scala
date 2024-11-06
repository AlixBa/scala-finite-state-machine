package com.github.alixba.fsm.framework

type Action[F[_], S] = S => F[Unit]
