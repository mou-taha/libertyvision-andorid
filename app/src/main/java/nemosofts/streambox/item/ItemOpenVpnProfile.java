package nemosofts.streambox.item;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;

public class ItemOpenVpnProfile {

    private final String id;
    private final String title;
    private final String username;
    private final String password;
    private final String config;
    private final String source;
    private final long createdAt;

    public ItemOpenVpnProfile(@NonNull String id,
                              @NonNull String title,
                              @Nullable String username,
                              @Nullable String password,
                              @NonNull String config,
                              @Nullable String source,
                              long createdAt) {
        this.id = id;
        this.title = title;
        this.username = username;
        this.password = password;
        this.config = config;
        this.source = source;
        this.createdAt = createdAt;
    }

    @NonNull
    public String getId() {
        return id;
    }

    @NonNull
    public String getTitle() {
        return title;
    }

    @Nullable
    public String getUsername() {
        return username;
    }

    @Nullable
    public String getPassword() {
        return password;
    }

    @NonNull
    public String getConfig() {
        return config;
    }

    @Nullable
    public String getSource() {
        return source;
    }

    public long getCreatedAt() {
        return createdAt;
    }
}
