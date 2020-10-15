# Put this in ~/.julia/config/startup.jl

atreplinit() do repl
  @async try
    sleep(0.1)
    @eval using Revise
    @async Revise.wait_steal_repl_backend()
  catch
    @warn("Could not load Revise.")
  end
end