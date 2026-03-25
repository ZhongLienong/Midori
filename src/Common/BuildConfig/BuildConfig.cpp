#include "Common/BuildConfig/BuildConfig.h"

#include <atomic>

namespace
{
    std::atomic<int> s_test_mode_override{ -1 };
}

namespace MidoriBuild
{
    ScopedTestModeOverride::ScopedTestModeOverride(bool enabled) noexcept
        : m_previous_value(s_test_mode_override.exchange(enabled ? 1 : 0, std::memory_order_acq_rel))
    {
    }

    ScopedTestModeOverride::~ScopedTestModeOverride() noexcept
    {
        s_test_mode_override.store(m_previous_value, std::memory_order_release);
    }

    bool IsTestMode() noexcept
    {
        const int override_value = s_test_mode_override.load(std::memory_order_acquire);
        if (override_value >= 0)
        {
            return override_value != 0;
        }

        static const bool s_is_test_mode = EnvironmentFlagEnabledUncached("MIDORI_TEST_MODE");
        return s_is_test_mode;
    }

    bool ShouldEmitInternalDiagnostics() noexcept
    {
        return !IsTestMode();
    }
}
