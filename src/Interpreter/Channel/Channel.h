#pragma once

#include "Interpreter/ValueTransfer/ValueTransfer.h"

#include <condition_variable>
#include <deque>
#include <mutex>
#include <optional>
#include <unordered_map>

class Channel
{
public:
	explicit Channel(int capacity);

	bool Send(SerializedValue message);

	std::optional<SerializedValue> Receive();

	std::optional<SerializedValue> TryReceive();

	void Close();

	bool IsClosed() const;

private:
	mutable std::mutex m_mutex;
	std::condition_variable m_not_empty;
	std::condition_variable m_not_full;
	std::deque<SerializedValue> m_queue;
	int m_capacity;
	bool m_closed = false;
};

class ChannelRegistry
{
public:
	static ChannelRegistry& GetInstance();

	int CreateChannel(int capacity);

	bool Send(int channel_id, SerializedValue message);

	std::optional<SerializedValue> Receive(int channel_id);

	std::optional<SerializedValue> TryReceive(int channel_id);

	void Close(int channel_id);

private:
	ChannelRegistry() = default;

	mutable std::mutex m_mutex;
	std::unordered_map<int, std::unique_ptr<Channel>> m_channels;
	int m_next_id = 1;

	Channel* FindChannel(int channel_id) const;
};
