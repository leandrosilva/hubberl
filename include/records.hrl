%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Records used by hubberl application.

%% for new ID generation
-record(counter, {key, value}).

%% business records
-record(queue, {id, doc}).
-record(topic, {id, doc}).
