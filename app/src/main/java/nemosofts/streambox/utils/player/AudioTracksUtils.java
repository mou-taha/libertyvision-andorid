package nemosofts.streambox.utils.player;

import android.content.Context;
import android.util.Pair;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AlertDialog;
import androidx.media3.common.C;
import androidx.media3.common.Format;
import androidx.media3.common.TrackSelectionOverride;
import androidx.media3.common.TrackSelectionParameters;
import androidx.media3.common.Tracks;
import androidx.media3.exoplayer.ExoPlayer;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Locale;

import nemosofts.streambox.R;
import nemosofts.streambox.utils.ApplicationUtil;

public class AudioTracksUtils {

    public interface AudioTrackSelectionListener {
        void onAudioTrackSelected(int groupIndex, int trackIndex);
    }

    public static void showAudioTracksDialog(Context context, ExoPlayer exoPlayer,
                                             AudioTrackSelectionListener listener) {
        if (exoPlayer == null) return;

        Tracks tracks = exoPlayer.getCurrentTracks();
        List<Tracks.Group> audioGroups = new ArrayList<>();

        for (Tracks.Group group : tracks.getGroups()) {
            if (group.getType() == C.TRACK_TYPE_AUDIO) {
                audioGroups.add(group);
            }
        }

        if (audioGroups.isEmpty()) {
            Toast.makeText(context, "No audio tracks available", Toast.LENGTH_SHORT).show();
            return;
        }

        AlertDialog.Builder builder = new AlertDialog.Builder(context);
        builder.setTitle("Select Audio Track");

        List<String> trackNames = new ArrayList<>();
        List<Pair<Integer, Integer>> groupTrackIndexList = new ArrayList<>();
        int selectedIndex = -1;

        for (int groupIndex = 0; groupIndex < audioGroups.size(); groupIndex++) {
            Tracks.Group group = audioGroups.get(groupIndex);
            for (int trackIndex = 0; trackIndex < group.length; trackIndex++) {
                Format format = group.getTrackFormat(trackIndex);
                trackNames.add(buildTrackName(context, format));
                groupTrackIndexList.add(new Pair<>(groupIndex, trackIndex));

                if (group.isTrackSelected(trackIndex)) {
                    selectedIndex = groupTrackIndexList.size() - 1;
                }
            }
        }

        builder.setSingleChoiceItems(trackNames.toArray(
                new String[0]),
                selectedIndex,
                (dialog, which) -> {
                    Pair<Integer, Integer> selectedPair = groupTrackIndexList.get(which);
                    listener.onAudioTrackSelected(selectedPair.first, selectedPair.second);
                    dialog.dismiss();
                });

        builder.setNegativeButton(android.R.string.cancel, null);
        builder.show();
    }

    @NonNull
    private static String buildTrackName(Context context, Format format) {
        if (format == null){
            return context.getString(R.string.track_unknown);
        }
        StringBuilder name = getStringBuilder(format);
        return ApplicationUtil.isEmpty(name, context.getString(R.string.track_unknown));
    }

    @NonNull
    private static StringBuilder getStringBuilder(@NonNull Format format) {
        StringBuilder name = new StringBuilder();
        if (format.label != null) {
            name.append(format.label);
        } else if (format.language != null) {
            try {
                Locale locale = new Locale.Builder()
                        .setLanguage(format.language.trim())
                        .build();
                String displayLanguage = locale.getDisplayLanguage();
                if (!displayLanguage.isEmpty()) {
                    name.append(displayLanguage);
                }
            } catch (IllegalArgumentException e) {
                name.append(format.language);
            }
        }

        if (format.channelCount != Format.NO_VALUE) {
            name.append(" (").append(format.channelCount).append("ch)");
        }

        if (format.sampleRate != Format.NO_VALUE) {
            name.append(" ").append(format.sampleRate / 1000).append("kHz");
        }
        return name;
    }

    public static void selectAudioTrack(ExoPlayer exoPlayer, int groupIndex, int trackIndex) {
        if (exoPlayer == null){
            return;
        }

        Tracks tracks = exoPlayer.getCurrentTracks();
        List<Tracks.Group> audioGroups = new ArrayList<>();

        for (Tracks.Group group : tracks.getGroups()) {
            if (group.getType() == C.TRACK_TYPE_AUDIO) {
                audioGroups.add(group);
            }
        }

        if (groupIndex >= audioGroups.size()){
            return;
        }

        Tracks.Group selectedGroup = audioGroups.get(groupIndex);
        TrackSelectionParameters.Builder builder = exoPlayer.getTrackSelectionParameters().buildUpon()
                .clearOverridesOfType(C.TRACK_TYPE_AUDIO)
                .setOverrideForType(new TrackSelectionOverride(
                                selectedGroup.getMediaTrackGroup(),
                                Collections.singletonList(trackIndex)
                ));
        exoPlayer.setTrackSelectionParameters(builder.build());
    }
}