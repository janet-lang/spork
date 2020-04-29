(import spork/rpc)

(rpc/server
  {:print (fn [self x] (print x))
   :eval (fn [self x] 
           # By default, server fibers are in an empty
           # environment, so eval is pretty much useless
           # (compile will almost always fail)
           (fiber/setenv
             (fiber/current)
             (table/setproto @{} root-env))
           (eval x))})
