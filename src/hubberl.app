%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

{application, hubberl,
 [{description, "hubberl"},
  {vsn, "0.01"},
  {modules, [
    hubberl,
    hubberl_app,
    hubberl_sup,
    hubberl_web,
    hubberl_web_admin,
    hubberl_web_client,
    hubberl_db,
    hubberl_deps
  ]},
  {registered, []},
  {mod, {hubberl_app, []}},
  {env, []},
  {applications, [kernel, stdlib, crypto, mnesia]}]}.
