
%
% ssh_kexinit: Connect to an ssh server, receive 
% the kexinit message and print it.
% 
% Execute in erl via 
% > c(ssh_kexinit), ssh_kexinit:main(Hostname).
%

-module(ssh_kexinit).

-export([main/1]).

-define(SSH_MSG_IGNORE ,  2).
-define(SSH_MSG_KEXINIT, 20).

main(Hostname) ->
	{ok, Socket} = gen_tcp:connect(Hostname, 22, [binary, {packet, 0}]),
	{ok, _} = recv(Socket),
	gen_tcp:send(Socket, "SSH-2.0-OpenSSH_6.1\r\n"),
	{ok, Bin} = recv_packet_plain_no_mac(Socket),
	handle(Bin).

recv(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			{ok, Bin};
		{tcp_closed, Socket} ->
			{closed, <<>>}
	end.

recv_packet_plain_no_mac(Socket) ->
	{ok, Bin} = recv(Socket),
	<<PacketLength:32, _/binary>> = Bin,
	recv_packet_plain_no_mac(Socket, PacketLength, Bin).

recv_packet_plain_no_mac(Socket, PacketLength, Bin) when byte_size(Bin) < (PacketLength + 4) ->
	{ok, Cin} = recv(Socket),
	recv_packet_plain_no_mac(Socket, PacketLength, <<Bin/binary, Cin/binary>>);
recv_packet_plain_no_mac(_Socket, _PacketLength, Bin) ->
	{ok, Bin}.

handle(<<_:32, _:8, ?SSH_MSG_IGNORE:8, _/binary>>) ->
	io:format(" SSH_MSG_IGNORE~n");
handle(Packet = <<_:32, _:8, ?SSH_MSG_KEXINIT:8, _/binary>>) ->
	io:format(" SSH_MSG_KEXINIT~n"),
	<<PacketLength:32, PaddingLength:8, _/binary>> = Packet,
	io:format("\tLength:        ~p~n", [byte_size(Packet)]),
	io:format("\tPacketLength:  ~p~n", [PacketLength]),
	io:format("\tPaddingLength: ~p~n", [PaddingLength]),
	N1 = PacketLength - PaddingLength - 1,
	N2 = PaddingLength,
	<<PacketLength:32, PaddingLength:8, PayLoad:N1/binary, _Padding:N2/binary>> = Packet,
	<<?SSH_MSG_KEXINIT:8, _Cookie:16/binary, Bin/binary>> = PayLoad,
	{NameLists, Rest} = extract_name_lists(Bin, 10),
	io:format("\tAlgorithms:~n\t~p~n\t~p~n\t~p~n\t~p~n\t~p~n\t~p~n\t~p~n\t~p~n\t~p~n\t~p~n", NameLists),
	<<FirstKexPacketFollows:8, _Unused:32>> = Rest,
	io:format("\tFirstKexPacketFollows: ~p~n", [FirstKexPacketFollows]),
	ok;
handle(_) ->
	ok.

extract_name_lists(Bin, Level) ->
	extract_name_lists(Bin, [], Level).

extract_name_lists(Bin, List, 0) ->
	{lists:reverse(List), Bin};
extract_name_lists(Bin, List, Level) ->
	<<Length:32, String:Length/binary, Rest/binary>> = Bin,
	extract_name_lists(Rest, [String | List], Level-1).

