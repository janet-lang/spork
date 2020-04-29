(import spork/rpc)

(rpc/server
  {:print (fn [x] (print x))
   :eval (fn [x] 
           # By default, server fibers are in an empty
           # environment, so eval is pretty much useless
           # (compile will almost always fail)
           (fiber/setenv
             (fiber/current)
             (table/setproto @{} root-env))
           (eval x))})
