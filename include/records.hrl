%% @author Leandro Silva <leandrodoze@gmail.com>
%% @copyright 2009 Leandro Silva.

%% @doc Records used by hubberl application.

%% for new ID generation
-record(counter, {key, value}).

%% business records
-record(destination, {uri, type, description}).
-record(message, {destination_uri, publisher_uri, content}).
-record(subscription, {destination_uri, subscriber_uri}).
