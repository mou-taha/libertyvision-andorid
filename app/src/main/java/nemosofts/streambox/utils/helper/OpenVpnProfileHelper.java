package nemosofts.streambox.utils.helper;

import android.content.Context;
import android.content.SharedPreferences;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

import com.google.gson.Gson;
import com.google.gson.reflect.TypeToken;

import java.lang.reflect.Type;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.UUID;

import nemosofts.streambox.item.ItemOpenVpnProfile;

public class OpenVpnProfileHelper {

    private static final String PREF_NAME = "open_vpn_profiles_store";
    private static final String KEY_PROFILES = "open_vpn_profiles";

    private final SharedPreferences preferences;
    private final Gson gson = new Gson();

    public OpenVpnProfileHelper(@NonNull Context context) {
        preferences = context.getSharedPreferences(PREF_NAME, Context.MODE_PRIVATE);
    }

    @NonNull
    public List<ItemOpenVpnProfile> getProfiles() {
        String json = preferences.getString(KEY_PROFILES, null);
        if (TextUtils.isEmpty(json)) {
            return new ArrayList<>();
        }
        Type type = new TypeToken<List<ItemOpenVpnProfile>>(){}.getType();
        List<ItemOpenVpnProfile> profiles = gson.fromJson(json, type);
        if (profiles == null) {
            return new ArrayList<>();
        }
        return new ArrayList<>(profiles);
    }

    public void saveProfile(@NonNull ItemOpenVpnProfile profile) {
        List<ItemOpenVpnProfile> profiles = getProfiles();
        profiles.add(0, profile);
        persist(profiles);
    }

    public void deleteProfile(@Nullable String profileId) {
        if (TextUtils.isEmpty(profileId)) {
            return;
        }
        List<ItemOpenVpnProfile> profiles = getProfiles();
        Iterator<ItemOpenVpnProfile> iterator = profiles.iterator();
        while (iterator.hasNext()) {
            ItemOpenVpnProfile profile = iterator.next();
            if (profileId.equals(profile.getId())) {
                iterator.remove();
                break;
            }
        }
        persist(profiles);
    }

    public void clear() {
        preferences.edit().remove(KEY_PROFILES).apply();
    }

    @NonNull
    public ItemOpenVpnProfile createProfile(@Nullable String title,
                                            @Nullable String username,
                                            @Nullable String password,
                                            @NonNull String config,
                                            @Nullable String source) {

        if (TextUtils.isEmpty(config) || TextUtils.isEmpty(source) || TextUtils.isEmpty(title)
                || TextUtils.isEmpty(username) || TextUtils.isEmpty(password)) {
            return new ItemOpenVpnProfile("", "", "",
                    "", "", "", System.currentTimeMillis()
            );
        }
        String resolvedTitle = !TextUtils.isEmpty(title) ? title : "";
        return new ItemOpenVpnProfile(
                UUID.randomUUID().toString(),
                resolvedTitle,
                username,
                password,
                config,
                source,
                System.currentTimeMillis()
        );
    }

    private void persist(@NonNull List<ItemOpenVpnProfile> profiles) {
        preferences.edit().putString(KEY_PROFILES, gson.toJson(profiles)).apply();
    }
}
