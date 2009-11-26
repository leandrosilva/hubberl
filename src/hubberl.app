{application, hubberl,
 [{description, "hubberl"},
  {vsn, "0.01"},
  {modules, [
    hubberl,
    hubberl_app,
    hubberl_sup,
    hubberl_web,
    hubberl_deps
  ]},
  {registered, []},
  {mod, {hubberl_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto]}]}.
