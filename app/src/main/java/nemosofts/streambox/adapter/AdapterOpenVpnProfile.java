package nemosofts.streambox.adapter;

import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemOpenVpnProfile;

public class AdapterOpenVpnProfile extends RecyclerView.Adapter<AdapterOpenVpnProfile.ProfileViewHolder> {

    public interface ProfileListener {
        void onProfileClicked(@NonNull ItemOpenVpnProfile profile);
        void onProfileLongClick(@NonNull ItemOpenVpnProfile profile);
    }

    private final List<ItemOpenVpnProfile> profiles;
    private final ProfileListener listener;

    public AdapterOpenVpnProfile(@NonNull List<ItemOpenVpnProfile> profiles,
                                 @NonNull ProfileListener listener) {
        this.profiles = profiles;
        this.listener = listener;
    }

    @NonNull
    @Override
    public ProfileViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View view = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.row_open_vpn_profile, parent, false);
        return new ProfileViewHolder(view);
    }

    @Override
    public void onBindViewHolder(@NonNull ProfileViewHolder holder, int position) {
        ItemOpenVpnProfile profile = profiles.get(position);
        holder.title.setText(profile.getTitle());
        String usernameText = profile.getUsername() != null
                ? profile.getUsername()
                : holder.username.getContext().getString(R.string.vpn_profile_unknown_user);
        holder.username.setText(usernameText);
        String sourceText = profile.getSource() != null
                ? profile.getSource()
                : holder.source.getContext().getString(R.string.vpn_profile_unknown_source);
        holder.source.setText(sourceText);
        holder.initial.setText(getInitial(profile.getTitle(), profile.getUsername()));

        holder.itemView.setOnClickListener(v -> listener.onProfileClicked(profile));
        holder.itemView.setOnLongClickListener(v -> {
            listener.onProfileLongClick(profile);
            return true;
        });
    }

    @Override
    public int getItemCount() {
        return profiles.size();
    }

    @NonNull
    private String getInitial(String title, String username) {
        String fallback = "V";
        if (title != null) {
            String trimmed = title.trim();
            if (!trimmed.isEmpty()) {
                return trimmed.substring(0, 1).toUpperCase();
            }
        }
        if (username != null) {
            String trimmedUser = username.trim();
            if (!trimmedUser.isEmpty()) {
                return trimmedUser.substring(0, 1).toUpperCase();
            }
        }
        return fallback;
    }

    static class ProfileViewHolder extends RecyclerView.ViewHolder {
        final TextView title;
        final TextView username;
        final TextView source;
        final TextView initial;

        ProfileViewHolder(@NonNull View itemView) {
            super(itemView);
            title = itemView.findViewById(R.id.tv_profile_title);
            username = itemView.findViewById(R.id.tv_profile_username);
            source = itemView.findViewById(R.id.tv_profile_source);
            initial = itemView.findViewById(R.id.tv_profile_initial);
        }
    }
}
