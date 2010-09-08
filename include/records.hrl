%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Records used by hubberl application.

%% for new ID generation
-record(counter, {key, value}).

%% business records
-record(destination, {name, description}).
-record(subscription, {destination_name, subscriber_uri}).
-record(message, {id, publisher_uri, content, delivered}). % delivered = atom() : not_yet | yes | no
