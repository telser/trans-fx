---
title: Effects with monad transformer transformers
---

Hi! This is a technical introduction to `trans-fx`, an effect framework library for Haskell. To say anything interesting I'll have to make a few assumptions about you, dear reader, and I think it's best to be up front about these. This document will make the most sense to you if:

1. You have some experience writing in a functional programming language, and a language with Hindley-Milner style type inference, and found these ideas to be to your liking.
2. You appreciate the use of monads to control side-effects and the use of monad transformers as a strategy for building complex monads from simpler ones.
3. You are invested in testing as a tool for building software that is responsive to changing requirements and resistant to decay.

* [Basic Effects](/trans-fx/basic-effects.html)
* [Doing IO](/trans-fx/doing-io.html)

> module Control.FX.Demo (
>     module Control.FX.Demo.BasicEffects
>   , module Control.FX.Demo.DoingIO
> ) where
> 
> import Control.FX.Demo.BasicEffects
> import Control.FX.Demo.DoingIO
