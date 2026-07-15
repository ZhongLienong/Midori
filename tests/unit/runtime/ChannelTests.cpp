#include <catch2/catch_test_macros.hpp>

#include "Interpreter/Channel/Channel.h"

#include <chrono>
#include <stop_token>
#include <thread>

TEST_CASE("Channel round-trips a value", "[channel]")
{
	Channel channel(2);
	SerializedValue message;
	message.m_raw_bits = 42u;

	REQUIRE(channel.Send(std::move(message), std::stop_token{}) == ChannelOpStatus::Ok);

	const ChannelReceiveResult received = channel.Receive(std::stop_token{});
	REQUIRE(received.m_status == ChannelOpStatus::Ok);
	REQUIRE(received.m_value.has_value());
	REQUIRE(received.m_value.value().m_raw_bits == 42u);
}

TEST_CASE("Blocked receive is cancelled by stop request", "[channel]")
{
	Channel channel(1);
	std::stop_source stop_source;

	ChannelReceiveResult received;
	std::thread receiver([&channel, &stop_source, &received]()
	{
		received = channel.Receive(stop_source.get_token());
	});

	std::this_thread::sleep_for(std::chrono::milliseconds(50));
	stop_source.request_stop();
	receiver.join();

	REQUIRE(received.m_status == ChannelOpStatus::Cancelled);
}

TEST_CASE("Blocked send is cancelled by stop request", "[channel]")
{
	Channel channel(1);
	SerializedValue first;
	REQUIRE(channel.Send(std::move(first), std::stop_token{}) == ChannelOpStatus::Ok);

	std::stop_source stop_source;
	ChannelOpStatus send_status = ChannelOpStatus::Ok;
	std::thread sender([&channel, &stop_source, &send_status]()
	{
		SerializedValue second;
		send_status = channel.Send(std::move(second), stop_source.get_token());
	});

	std::this_thread::sleep_for(std::chrono::milliseconds(50));
	stop_source.request_stop();
	sender.join();

	REQUIRE(send_status == ChannelOpStatus::Cancelled);
}

TEST_CASE("Close unblocks a pending receive with Closed status", "[channel]")
{
	Channel channel(1);
	ChannelReceiveResult received;
	std::thread receiver([&channel, &received]()
	{
		received = channel.Receive(std::stop_token{});
	});

	std::this_thread::sleep_for(std::chrono::milliseconds(50));
	channel.Close();
	receiver.join();

	REQUIRE(received.m_status == ChannelOpStatus::Closed);
}

TEST_CASE("Send to a closed channel reports Closed", "[channel]")
{
	Channel channel(1);
	channel.Close();

	SerializedValue message;
	REQUIRE(channel.Send(std::move(message), std::stop_token{}) == ChannelOpStatus::Closed);
}
