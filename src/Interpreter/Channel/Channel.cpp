#include "Channel.h"

#include <memory>

Channel::Channel(int capacity)
	: m_capacity(capacity > 0 ? capacity : 1)
{
}

bool Channel::Send(SerializedValue message)
{
	std::unique_lock<std::mutex> lock(m_mutex);
	m_not_full.wait(lock, [this]()
	{
		return m_closed || static_cast<int>(m_queue.size()) < m_capacity;
	});

	if (m_closed)
	{
		return false;
	}

	m_queue.push_back(std::move(message));
	m_not_empty.notify_one();
	return true;
}

std::optional<SerializedValue> Channel::Receive()
{
	std::unique_lock<std::mutex> lock(m_mutex);
	m_not_empty.wait(lock, [this]()
	{
		return !m_queue.empty() || m_closed;
	});

	if (m_queue.empty())
	{
		return std::nullopt;
	}

	SerializedValue message = std::move(m_queue.front());
	m_queue.pop_front();
	m_not_full.notify_one();
	return message;
}

std::optional<SerializedValue> Channel::TryReceive()
{
	std::lock_guard<std::mutex> lock(m_mutex);
	if (m_queue.empty())
	{
		return std::nullopt;
	}

	SerializedValue message = std::move(m_queue.front());
	m_queue.pop_front();
	m_not_full.notify_one();
	return message;
}

void Channel::Close()
{
	std::lock_guard<std::mutex> lock(m_mutex);
	m_closed = true;
	m_not_empty.notify_all();
	m_not_full.notify_all();
}

bool Channel::IsClosed() const
{
	std::lock_guard<std::mutex> lock(m_mutex);
	return m_closed;
}

ChannelRegistry& ChannelRegistry::GetInstance()
{
	static ChannelRegistry instance;
	return instance;
}

int ChannelRegistry::CreateChannel(int capacity)
{
	std::lock_guard<std::mutex> lock(m_mutex);
	const int id = m_next_id;
	m_next_id += 1;
	m_channels.emplace(id, std::make_unique<Channel>(capacity));
	return id;
}

bool ChannelRegistry::Send(int channel_id, SerializedValue message)
{
	Channel* channel = FindChannel(channel_id);
	if (channel == nullptr)
	{
		return false;
	}
	return channel->Send(std::move(message));
}

std::optional<SerializedValue> ChannelRegistry::Receive(int channel_id)
{
	Channel* channel = FindChannel(channel_id);
	if (channel == nullptr)
	{
		return std::nullopt;
	}
	return channel->Receive();
}

std::optional<SerializedValue> ChannelRegistry::TryReceive(int channel_id)
{
	Channel* channel = FindChannel(channel_id);
	if (channel == nullptr)
	{
		return std::nullopt;
	}
	return channel->TryReceive();
}

void ChannelRegistry::Close(int channel_id)
{
	Channel* channel = FindChannel(channel_id);
	if (channel != nullptr)
	{
		channel->Close();
	}
}

Channel* ChannelRegistry::FindChannel(int channel_id) const
{
	std::lock_guard<std::mutex> lock(m_mutex);
	std::unordered_map<int, std::unique_ptr<Channel>>::const_iterator channel_it = m_channels.find(channel_id);
	if (channel_it == m_channels.end())
	{
		return nullptr;
	}
	return channel_it->second.get();
}
