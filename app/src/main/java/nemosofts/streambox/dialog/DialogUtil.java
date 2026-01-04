package nemosofts.streambox.dialog;

import static android.view.WindowManager.LayoutParams.MATCH_PARENT;
import static android.view.WindowManager.LayoutParams.WRAP_CONTENT;

import android.app.Activity;
import android.app.Dialog;
import android.content.Context;
import android.content.Intent;
import android.net.Uri;
import android.os.Handler;
import android.os.Looper;
import android.view.KeyEvent;
import android.view.View;
import android.view.Window;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.RadioGroup;
import android.widget.TextView;
import android.widget.Toast;

import androidx.annotation.OptIn;
import androidx.core.app.ActivityCompat;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.material.ImageViewRound;

import androidx.nemosofts.utils.DeviceUtils;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.CatchUpActivity;
import nemosofts.streambox.activity.CategoriesActivity;
import nemosofts.streambox.activity.DownloadActivity;
import nemosofts.streambox.activity.MultipleScreenActivity;
import nemosofts.streambox.activity.ProfileActivity;
import nemosofts.streambox.activity.SettingActivity;
import nemosofts.streambox.activity.UsersListActivity;
import nemosofts.streambox.activity.WebActivity;
import nemosofts.streambox.adapter.AdapterMenu;
import nemosofts.streambox.adapter.AdapterRadioButton;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.item.ItemRadioButton;
import nemosofts.streambox.item.ItemSetting;
import nemosofts.streambox.utils.IfSupported;
import nemosofts.streambox.utils.helper.JSHelper;
import nemosofts.streambox.utils.helper.SPHelper;

public class DialogUtil {

    private static Dialog dialog;
    
    private static Boolean flag = false;
    private static Boolean getIsFlag() {
        return flag;
    }
    private static void setIsFlag(Boolean isFlag) {
        flag = isFlag;
    }

    private DialogUtil() {
        throw new IllegalStateException("Utility class");
    }

    // Dialog --------------------------------------------------------------------------------------
    public static void exitDialog(Activity activity) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }

        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        boolean isTvBox = DeviceUtils.isTvBox(activity);
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        if (isTvBox){
            dialog.setContentView(R.layout.dialog_app_tv);
            dialog.findViewById(R.id.tv_do_cancel).setOnClickListener(view -> dismissDialog());
            dialog.findViewById(R.id.tv_do_yes).setOnClickListener(view -> {
                dismissDialog();
                activity.finish();
            });
            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            dialog.findViewById(R.id.tv_do_cancel).requestFocus();
        } else {
            dialog.setContentView(R.layout.dialog_app);

            ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_exit_to_app);

            // Optional shimmer animation or vector tint
            icon.animate().rotation(10).setDuration(300).withEndAction(() ->
                    icon.animate().rotation(0).setDuration(300)
            );

            TextView title = dialog.findViewById(R.id.tv_dialog_title);
            title.setText(R.string.exit);

            TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
            msg.setText(R.string.sure_exit);

            dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> dismissDialog());

            dialog.findViewById(R.id.tv_dialog_no).setOnClickListener(view -> dismissDialog());
            dialog.findViewById(R.id.tv_dialog_yes).setOnClickListener(view -> {
                dismissDialog();
                activity.finish();
            });

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
        }
        Window window = dialog.getWindow();
        if (window != null){
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void maintenanceDialog(Activity activity) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_app);

        ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
        icon.setImageResource(R.drawable.ic_error);

        // Optional shimmer animation or vector tint
        icon.animate().rotation(10).setDuration(300).withEndAction(() ->
                icon.animate().rotation(0).setDuration(300)
        );

        TextView title = dialog.findViewById(R.id.tv_dialog_title);
        title.setText(R.string.maintenance);

        TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
        msg.setText(R.string.we_are_performing_scheduled);

        // VISIBLE
        TextView titleSub = dialog.findViewById(R.id.tv_dialog_title_sub);
        titleSub.setVisibility(View.VISIBLE);
        titleSub.setText(R.string.temporarily_down_for_maintenance);

        dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> {
            dismissDialog();
            activity.finish();
        });

        TextView no = dialog.findViewById(R.id.tv_dialog_no);
        no.setText(R.string.cancel);
        no.setOnClickListener(view -> {
            dismissDialog();
            activity.finish();
        });

        TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
        yes.setVisibility(View.GONE);

        View view = dialog.findViewById(R.id.vw_dialog_bar);
        view.setVisibility(View.GONE);

        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        if (window != null){
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void upgradeDialog(Activity activity, CancelListener listener) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_app);

        ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
        icon.setImageResource(R.drawable.ic_error);

        // Optional shimmer animation or vector tint
        icon.animate().rotation(10).setDuration(300).withEndAction(() ->
                icon.animate().rotation(0).setDuration(300)
        );

        TextView title = dialog.findViewById(R.id.tv_dialog_title);
        title.setText(R.string.upgrade);

        TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
        msg.setText(R.string.its_time_to_upgrade);

        // VISIBLE
        TextView titleSub = dialog.findViewById(R.id.tv_dialog_title_sub);
        titleSub.setVisibility(View.VISIBLE);
        titleSub.setText(R.string.upgrade);

        dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> {
            dismissDialog();
            listener.onCancel();
        });

        TextView no = dialog.findViewById(R.id.tv_dialog_no);
        no.setText(R.string.cancel);
        no.setOnClickListener(view -> {
            dismissDialog();
            listener.onCancel();
        });

        TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
        yes.setText(R.string.do_it_now);
        yes.setOnClickListener(view -> {
            dismissDialog();
            if (!Callback.getAppRedirectUrl().isEmpty()){
                activity.startActivity(new Intent(Intent.ACTION_VIEW, Uri.parse(Callback.getAppRedirectUrl())));
            }
        });

        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        if (window != null){
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void dModeDialog(Activity activity) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_app);

        ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
        icon.setImageResource(R.drawable.ic_error);

        // Optional shimmer animation or vector tint
        icon.animate().rotation(10).setDuration(300).withEndAction(() ->
                icon.animate().rotation(0).setDuration(300)
        );

        TextView title = dialog.findViewById(R.id.tv_dialog_title);
        title.setText(R.string.developer_mode);

        TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
        msg.setText(R.string.turn_off_developer_mode);

        // VISIBLE
        TextView titleSub = dialog.findViewById(R.id.tv_dialog_title_sub);
        titleSub.setVisibility(View.VISIBLE);
        titleSub.setText(R.string.developer_mode);

        dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> {
            dismissDialog();
            activity.finish();
        });

        TextView no = dialog.findViewById(R.id.tv_dialog_no);
        no.setText(R.string.try_again_later);
        no.setOnClickListener(view -> {
            dismissDialog();
            activity.finish();
        });

        TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
        yes.setVisibility(View.GONE);

        View view = dialog.findViewById(R.id.vw_dialog_bar);
        view.setVisibility(View.GONE);

        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        if (window != null){
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void vpnDialog(Activity activity) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_app);

        ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
        icon.setImageResource(R.drawable.ic_error);

        // Optional shimmer animation or vector tint
        icon.animate().rotation(10).setDuration(300).withEndAction(() ->
                icon.animate().rotation(0).setDuration(300)
        );

        TextView title = dialog.findViewById(R.id.tv_dialog_title);
        title.setText(R.string.sniffing_detected);

        TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
        msg.setText(R.string.turn_off_all_sniffers_tools);

        // VISIBLE
        TextView titleSub = dialog.findViewById(R.id.tv_dialog_title_sub);
        titleSub.setVisibility(View.VISIBLE);
        titleSub.setText(R.string.sniffing_detected);

        dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> {
            dismissDialog();
            activity.finish();
        });

        TextView no = dialog.findViewById(R.id.tv_dialog_no);
        no.setText(R.string.cancel);
        no.setOnClickListener(view -> {
            dismissDialog();
            activity.finish();
        });

        TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
        yes.setVisibility(View.GONE);

        View view = dialog.findViewById(R.id.vw_dialog_bar);
        view.setVisibility(View.GONE);

        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        if (window != null){
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void logoutDialog(Activity activity, LogoutListener logoutListener) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        boolean isTvBox = DeviceUtils.isTvBox(activity);
        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        if (isTvBox){
            dialog.setContentView(R.layout.dialog_app_tv);

            ImageView icon =  dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_exit_to_app);

            // Optional shimmer animation or vector tint
            icon.animate().rotation(10).setDuration(300).withEndAction(() ->
                    icon.animate().rotation(0).setDuration(300)
            );

            TextView desc =  dialog.findViewById(R.id.tv_dialog_desc);
            desc.setText(R.string.sure_logout);

            TextView btnYes = dialog.findViewById(R.id.tv_do_yes);
            btnYes.setText(R.string.yes);
            btnYes.setOnClickListener(view -> {
                dismissDialog();
                logoutListener.onLogout();
            });

            TextView btnCancel = dialog.findViewById(R.id.tv_do_cancel);
            btnCancel.setText(R.string.no);
            btnCancel.setOnClickListener(view -> dismissDialog());

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            dialog.findViewById(R.id.tv_do_cancel).requestFocus();
        } else {

            dialog.setContentView(R.layout.dialog_app);

            ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_exit_to_app);

            // Optional shimmer animation or vector tint
            icon.animate().rotation(10).setDuration(300).withEndAction(() ->
                    icon.animate().rotation(0).setDuration(300)
            );

            TextView title = dialog.findViewById(R.id.tv_dialog_title);
            title.setText(R.string.logout);

            TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
            msg.setText(R.string.sure_logout);

            dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> dismissDialog());

            TextView no = dialog.findViewById(R.id.tv_dialog_no);
            no.setText(R.string.no);
            no.setOnClickListener(view -> dismissDialog());

            TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
            yes.setText(R.string.yes);
            yes.setOnClickListener(view -> {
                dismissDialog();
                logoutListener.onLogout();
            });

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
        }
        Window window = dialog.getWindow();
        if (window != null){
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void deleteDialog(Context context, DeleteListener listener) {
        if (context == null) {
            return; // Avoid showing dialog if context is not valid
        }
        boolean isTvBox = DeviceUtils.isTvBox(context);
        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        dialog = new Dialog(context);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        if (isTvBox){
            dialog.setContentView(R.layout.dialog_app_tv);

            ImageView icon =  dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_trash);

            // Optional shimmer animation or vector tint
            icon.animate().rotation(10).setDuration(300).withEndAction(() ->
                    icon.animate().rotation(0).setDuration(300)
            );

            TextView desc =  dialog.findViewById(R.id.tv_dialog_desc);
            desc.setText(R.string.sure_delete);

            TextView btnYes = dialog.findViewById(R.id.tv_do_yes);
            btnYes.setText(R.string.delete);
            btnYes.setOnClickListener(view -> {
                dismissDialog();
                listener.onDelete();
            });

            TextView btnCancel = dialog.findViewById(R.id.tv_do_cancel);
            btnCancel.setText(R.string.cancel);
            btnCancel.setOnClickListener(view -> dismissDialog());

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            dialog.findViewById(R.id.tv_do_cancel).requestFocus();

        } else {

            dialog.setContentView(R.layout.dialog_app);

            ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_trash);

            // Optional shimmer animation or vector tint
            icon.animate().rotation(10).setDuration(300).withEndAction(() ->
                    icon.animate().rotation(0).setDuration(300)
            );

            TextView title = dialog.findViewById(R.id.tv_dialog_title);
            title.setText(R.string.delete);

            TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
            msg.setText(R.string.sure_delete);

            dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> dismissDialog());

            TextView no = dialog.findViewById(R.id.tv_dialog_no);
            no.setText(R.string.cancel);
            no.setOnClickListener(view -> dismissDialog());

            TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
            yes.setText(R.string.delete);
            yes.setOnClickListener(view -> {
                dismissDialog();
                listener.onDelete();
            });

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
        }
        Window window = dialog.getWindow();
        if (window != null){
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void downloadDataDialog(Activity activity, String type, DownloadListener listener) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        boolean isTvBox = DeviceUtils.isTvBox(activity);
        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        if (isTvBox){
            dialog.setContentView(R.layout.dialog_app_tv);

            ImageView icon =  dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_reset);

            // Optional shimmer animation or vector tint
            icon.animate().rotation(10).setDuration(300).withEndAction(() ->
                    icon.animate().rotation(0).setDuration(300)
            );

            TextView desc =  dialog.findViewById(R.id.tv_dialog_desc);
            desc.setText(R.string.sure_reload_data);

            TextView btnYes = dialog.findViewById(R.id.tv_do_yes);
            btnYes.setText(R.string.yes);
            btnYes.setOnClickListener(view -> {
                dismissDialog();
                listener.onDownload(type);
            });

            TextView btnCancel = dialog.findViewById(R.id.tv_do_cancel);
            btnCancel.setText(R.string.no);
            btnCancel.setOnClickListener(view -> dismissDialog());

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            dialog.findViewById(R.id.tv_do_cancel).requestFocus();
        } else {

            dialog.setContentView(R.layout.dialog_app);

            ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_reset);

            // Optional shimmer animation or vector tint
            icon.animate().rotation(10).setDuration(300).withEndAction(() ->
                    icon.animate().rotation(0).setDuration(300)
            );

            TextView title = dialog.findViewById(R.id.tv_dialog_title);
            title.setText(R.string.reload_data);

            TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
            msg.setText(R.string.sure_reload_data);

            dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> dismissDialog());

            TextView no = dialog.findViewById(R.id.tv_dialog_no);
            no.setText(R.string.no);
            no.setOnClickListener(view -> dismissDialog());

            TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
            yes.setText(R.string.yes);
            yes.setOnClickListener(view -> {
                dismissDialog();
                listener.onDownload(type);
            });

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
        }
        Window window = dialog.getWindow();
        if (window != null){
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void screenDialog(Activity activity, ScreenDialogListener listener) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_screen);
        dialog.setCancelable(false);
        dialog.findViewById(R.id.iv_screen_one).setOnClickListener(v -> {
            listener.onSubmit(1);
            dismissDialog();
        });
        dialog.findViewById(R.id.iv_screen_two).setOnClickListener(v -> {
            listener.onSubmit(2);
            dismissDialog();
        });
        dialog.findViewById(R.id.iv_screen_three).setOnClickListener(v -> {
            listener.onSubmit(3);
            dismissDialog();
        });
        dialog.findViewById(R.id.iv_screen_four).setOnClickListener(v -> {
            listener.onSubmit(4);
            dismissDialog();
        });
        dialog.findViewById(R.id.iv_screen_five).setOnClickListener(v -> {
            listener.onSubmit(5);
            dismissDialog();
        });
        dialog.setOnKeyListener((dialog, keyCode, event) -> {
            if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)) {
                listener.onCancel();
                dialog.dismiss();
                return true;
            }
            return false;
        });
        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        if (window != null){
            IfSupported.hideStatusBarDialog(window);
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void liveDownloadDialog(Context context, LiveDownloadListener listener) {
        if (context == null) {
            return; // Avoid showing dialog if context is not valid
        }
        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        boolean isTvBox = DeviceUtils.isTvBox(context);
        dialog = new Dialog(context);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        if (isTvBox){
            dialog.setContentView(R.layout.dialog_app_tv);

            ImageView icon =  dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_file_download);

            // Optional shimmer animation or vector tint
            icon.animate().rotation(10).setDuration(300).withEndAction(() ->
                    icon.animate().rotation(0).setDuration(300)
            );

            TextView desc =  dialog.findViewById(R.id.tv_dialog_desc);
            desc.setText(R.string.want_to_download);

            TextView btnYes = dialog.findViewById(R.id.tv_do_yes);
            btnYes.setText(R.string.yes);
            btnYes.setOnClickListener(view -> {
                listener.onDownload();
                dismissDialog();
            });

            TextView btnCancel = dialog.findViewById(R.id.tv_do_cancel);
            btnCancel.setText(R.string.no);
            btnCancel.setOnClickListener(view -> dismissDialog());

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            dialog.findViewById(R.id.tv_do_cancel).requestFocus();

        } else {
            dialog.setContentView(R.layout.dialog_app);

            ImageView icon = dialog.findViewById(R.id.iv_dialog_icon);
            icon.setImageResource(R.drawable.ic_file_download);

            // Optional shimmer animation or vector tint
            icon.animate().rotation(10).setDuration(300).withEndAction(() ->
                    icon.animate().rotation(0).setDuration(300)
            );

            TextView title = dialog.findViewById(R.id.tv_dialog_title);
            title.setText(R.string.live_not_downloaded);

            TextView msg = dialog.findViewById(R.id.tv_dialog_msg);
            msg.setText(R.string.want_to_download);

            dialog.findViewById(R.id.iv_dialog_close).setOnClickListener(view -> dismissDialog());

            TextView no = dialog.findViewById(R.id.tv_dialog_no);
            no.setText(R.string.no);
            no.setOnClickListener(view -> dismissDialog());

            TextView yes = dialog.findViewById(R.id.tv_dialog_yes);
            yes.setText(R.string.yes);
            yes.setOnClickListener(view -> {
                dismissDialog();
                listener.onDownload();
            });

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
        }
        Window window = dialog.getWindow();
        if (window != null){
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void radioBtnDialog(Context context, List<ItemRadioButton> arrayList, int position,
                                      String pageTitle, RadioBtnListener radioBtnListener) {
        if (context == null) {
            return; // Avoid showing dialog if context is not valid
        }
        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        dialog = new Dialog(context);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_radio_btn);

        TextView title = dialog.findViewById(R.id.tv_page_title_dil);
        title.setText(pageTitle);

        RecyclerView rv = dialog.findViewById(R.id.rv_radio_btn);
        GridLayoutManager grid = new GridLayoutManager(context, 2);
        grid.setSpanCount(2);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());

        AdapterRadioButton adapter = new AdapterRadioButton(context, arrayList, position);
        rv.setAdapter(adapter);

        dialog.findViewById(R.id.tv_submit).setOnClickListener(view -> {
            radioBtnListener.onSetLimit(adapter.getData());
            dismissDialog();
        });
        dialog.findViewById(R.id.iv_close).setOnClickListener(view -> dismissDialog());
        dialog.findViewById(R.id.tv_cancel).setOnClickListener(view -> dismissDialog());

        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        if (window != null){
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void popupAdsDialog(Activity activity) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        if (!Callback.getAdsImage().isEmpty() && !Callback.getAdsRedirectURL().isEmpty()){
            dialog = new Dialog(activity);
            dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
            dialog.setContentView(R.layout.dialog_popup_ads);

            ImageViewRound loadAds = dialog.findViewById(R.id.iv_ads);
            ProgressBar pb  = dialog.findViewById(R.id.pb_ads);
            dialog.findViewById(R.id.vw_ads).setOnClickListener(v -> {
                if (Callback.getAdsRedirectType().equals("external")){
                    Intent browserIntent = new Intent(Intent.ACTION_VIEW, Uri.parse(Callback.getAdsRedirectURL()));
                    activity.startActivity(browserIntent);
                } else {
                    Intent intent = new Intent(activity, WebActivity.class);
                    intent.putExtra("web_url", Callback.getAdsRedirectURL());
                    intent.putExtra("page_title", Callback.getAdsTitle());
                    ActivityCompat.startActivity(activity, intent, null);
                }
            });
            Picasso.get()
                    .load(Callback.getAdsImage().replace(" ", "%20"))
                    .placeholder(R.color.white)
                    .error(R.color.white)
                    .into(loadAds, new com.squareup.picasso.Callback() {
                        @Override
                        public void onSuccess() {
                            pb.setVisibility(View.GONE);
                        }

                        @Override
                        public void onError(Exception e) {
                            pb.setVisibility(View.GONE);
                        }
                    });

            dialog.findViewById(R.id.iv_back_ads).setOnClickListener(view -> {
                dismissDialog();
                Callback.setAdsImage("");
                Callback.setAdsRedirectURL("");
            });

            boolean isTvBox = DeviceUtils.isTvBox(activity);
            if (isTvBox){
                dialog.findViewById(R.id.iv_back_ads).requestFocus();
            }

            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            Window window = dialog.getWindow();
            if (window != null){
                window.setLayout(MATCH_PARENT, WRAP_CONTENT);
            }
        }
    }

    public static void childCountDialog(Context context, int pos,  ChildCountListener listener) {
        if (context == null) {
            return; // Avoid showing dialog if context is not valid
        }
        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        SPHelper spHelper = new SPHelper(context);
        if (spHelper.getAdultPassword().isEmpty()){
            listener.onUnLock(pos);
        } else {
            dialog = new Dialog(context);
            dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
            dialog.setContentView(R.layout.dialog_child_count);
            dialog.findViewById(R.id.iv_close_adult).setOnClickListener(view -> dismissDialog());
            dialog.findViewById(R.id.tv_cancel_adult).setOnClickListener(view -> dismissDialog());
            EditText password = dialog.findViewById(R.id.et_password_adult);
            dialog.findViewById(R.id.tv_unlock_adult).setOnClickListener(view -> {
                if(password.getText().toString().trim().isEmpty()) {
                    password.setError(context.getString(R.string.err_cannot_empty));
                    password.requestFocus();
                } else {
                    if (spHelper.getAdultPassword().equals(password.getText().toString())){
                        listener.onUnLock(pos);
                        dismissDialog();
                    } else {
                        password.setError(context.getString(R.string.err_password));
                        password.requestFocus();
                    }
                }
            });
            Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
            dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
            dialog.show();
            Window window = dialog.getWindow();
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    public static void filterDialog(Context context, int pageType, FilterDialogListener listener) {
        if (context == null) {
            return; // Avoid showing dialog if context is not valid
        }
        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        JSHelper jsHelper = new JSHelper(context);
        dialog = new Dialog(context);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);

        dialog.setContentView(R.layout.dialog_filter);
        dialog.findViewById(R.id.iv_close_btn).setOnClickListener(view -> dismissDialog());

        setIsFlag(false);
        if (pageType == 1){
            flag = jsHelper.getIsLiveOrder();
        } else if (pageType == 2){
            flag = jsHelper.getIsMovieOrder();
        }  else if (pageType == 3){
            flag = jsHelper.getIsSeriesOrder();
        }  else if (pageType == 4){
            flag = jsHelper.getIsEpisodesOrder();
        }

        RadioGroup rg = dialog.findViewById(R.id.rg);
        if (Boolean.TRUE.equals(flag)){
            rg.check(R.id.rd_1);
        } else {
            rg.check(R.id.rd_2);
        }

        dialog.findViewById(R.id.rd_1).setOnClickListener(view -> setIsFlag(true));
        dialog.findViewById(R.id.rd_2).setOnClickListener(view -> setIsFlag(false));

        dialog.findViewById(R.id.btn_cancel_filter).setOnClickListener(view -> {
            if (pageType == 1){
                jsHelper.setIsLiveOrder(false);
            } else if (pageType == 2){
                jsHelper.setIsMovieOrder(false);
            } else if (pageType == 3){
                jsHelper.setIsSeriesOrder(false);
            } else if (pageType == 4){
                jsHelper.setIsEpisodesOrder(false);
            }
            listener.onSubmit();
            dismissDialog();
        });
        dialog.findViewById(R.id.btn_submit_filter).setOnClickListener(view -> {
            if (pageType == 1){
                jsHelper.setIsLiveOrder(getIsFlag());
            } else if (pageType == 2){
                jsHelper.setIsMovieOrder(getIsFlag());
            } else if (pageType == 3){
                jsHelper.setIsSeriesOrder(getIsFlag());
            } else if (pageType == 4){
                jsHelper.setIsEpisodesOrder(getIsFlag());
            }
            listener.onSubmit();
            dismissDialog();
        });

        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        window.setLayout(MATCH_PARENT, WRAP_CONTENT);
    }

    public static void wallpaperDialog(Activity activity, String filePath) {
        if (activity == null || activity.isFinishing() || filePath.isEmpty()) {
            return; // Avoid showing dialog if activity is not valid
        }
        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_wallpaper);

        ImageViewRound loadAds = dialog.findViewById(R.id.iv_wallpaper);
        ProgressBar pb  = dialog.findViewById(R.id.pb);
        Picasso.get()
                .load("https://image.tmdb.org/t/p/original"+filePath)
                .into(loadAds, new com.squareup.picasso.Callback() {
                    @Override
                    public void onSuccess() {
                        pb.setVisibility(View.GONE);
                    }

                    @Override
                    public void onError(Exception e) {
                        pb.setVisibility(View.GONE);
                        dismissDialog();
                    }
                });

        dialog.findViewById(R.id.iv_back_wallpaper).setOnClickListener(v -> dismissDialog());
        dialog.findViewById(R.id.rl_root_dialog).setOnClickListener(v -> dismissDialog());
        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        Window window = dialog.getWindow();
        if (window != null){
            window.setLayout(MATCH_PARENT, MATCH_PARENT);
        }
    }

    public static void menuDialog(Activity activity, DeleteListener listener) {
        if (activity == null || activity.isFinishing()) {
            return; // Avoid showing dialog if activity is not valid
        }

        if (dialog != null){
            dismissDialog();
            dialog = null;
        }
        dialog = new Dialog(activity);
        dialog.requestWindowFeature(Window.FEATURE_NO_TITLE);
        dialog.setContentView(R.layout.dialog_menu);
        dialog.setCancelable(true);

        dialog.findViewById(R.id.ll_close).setOnClickListener(view -> dismissDialog());
        dialog.findViewById(R.id.ll_dialog).setOnClickListener(view -> {
            // noCancel
        });

        RecyclerView rv = dialog.findViewById(R.id.rv_dialog);

        GridLayoutManager grid;
        grid = new GridLayoutManager(activity, 2);
        grid.setSpanCount(2);
        rv.setLayoutManager(grid);
        rv.setNestedScrollingEnabled(false);

        ArrayList<ItemSetting> arrayList = new ArrayList<>();
        arrayList.add(new ItemSetting(activity.getResources().getString(R.string.profile), R.drawable.ic_outline_person));
        arrayList.add(new ItemSetting(activity.getResources().getString(R.string.logout), R.drawable.ic_people));
        arrayList.add(new ItemSetting(activity.getResources().getString(R.string.download), R.drawable.iv_downloading));
        arrayList.add(new ItemSetting(activity.getResources().getString(R.string.settings), R.drawable.ic_player_setting));
        if (!new SPHelper(activity).getCurrent(Callback.TAG_TV).isEmpty()) {
            arrayList.add(new ItemSetting(activity.getResources().getString(R.string.menu_live_with_epg), R.drawable.ic_note));
            arrayList.add(new ItemSetting(activity.getResources().getString(R.string.menu_catch_up), R.drawable.ic_timer_start));
            arrayList.add(new ItemSetting(activity.getResources().getString(R.string.menu_multiple_screen), R.drawable.ic_multiple_screen));
        }
        arrayList.add(new ItemSetting(activity.getResources().getString(R.string.reload_data), R.drawable.ic_reset));

        AdapterMenu adapter = new AdapterMenu(activity, arrayList, (itemSerials, position) ->
                setOnClick(activity, arrayList.get(position).getName(), listener)
        );
        rv.setAdapter(adapter);
        Objects.requireNonNull(dialog.getWindow()).setBackgroundDrawableResource(android.R.color.transparent);
        dialog.getWindow().getAttributes().windowAnimations = R.style.dialogAnimation;
        dialog.show();
        if (DeviceUtils.isTvBox(activity)){
            rv.requestFocus();
        }
        Window window = dialog.getWindow();
        if (window != null){
            window.setLayout(MATCH_PARENT, WRAP_CONTENT);
        }
    }

    @OptIn(markerClass = UnstableApi.class)
    private static void setOnClick(Activity activity, String name, DeleteListener listener) {
        if (name == null){
            return;
        }

        dismissDialog();

        // Create a map of action names to intents or actions
        Map<String, Runnable> actionMap = new HashMap<>();

        actionMap.put(activity.getResources().getString(R.string.profile), () ->
                activity.startActivity(new Intent(activity, ProfileActivity.class))
        );
        actionMap.put(activity.getResources().getString(R.string.logout), () ->
                signOut(activity)
        );
        actionMap.put(activity.getResources().getString(R.string.settings), () ->
                activity.startActivity(new Intent(activity, SettingActivity.class))
        );
        actionMap.put(activity.getResources().getString(R.string.download), () ->
                activity.startActivity(new Intent(activity, DownloadActivity.class))
        );
        actionMap.put(activity.getResources().getString(R.string.menu_live_with_epg), () ->
                activity.startActivity(new Intent(activity, CategoriesActivity.class))
        );
        actionMap.put(activity.getResources().getString(R.string.menu_catch_up), () ->
                activity.startActivity(new Intent(activity, CatchUpActivity.class))
        );
        actionMap.put(activity.getResources().getString(R.string.menu_multiple_screen), () ->
                activity.startActivity(new Intent(activity, MultipleScreenActivity.class))
        );

        actionMap.put(activity.getResources().getString(R.string.reload_data), () ->{
            new JSHelper(activity).removeAllData();
            SPHelper spHelper = new SPHelper(activity);
            spHelper.setCurrentDateEmpty(Callback.TAG_TV);
            spHelper.setCurrentDateEmpty(Callback.TAG_MOVIE);
            spHelper.setCurrentDateEmpty(Callback.TAG_SERIES);
            if (listener != null){
                listener.onDelete();
            } else {
                activity.recreate();
            }
        });

        // Execute the corresponding action if found
        Runnable action = actionMap.get(name);
        if (action != null) {
            action.run();
        }
    }

    private static void signOut(Activity activity) {
        DialogUtil.logoutDialog(activity, () -> {
            Intent intent = new Intent(activity, UsersListActivity.class);
            SPHelper spHelper = new SPHelper(activity);
            if (spHelper.isLogged()) {
                new JSHelper(activity).removeAllData();
                spHelper.removeSignOut();
                intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK);
                intent.putExtra("from", "");
                Toast.makeText(activity, activity.getString(R.string.logout_success), Toast.LENGTH_SHORT).show();
            } else {
                intent.putExtra("from", "app");
            }
            activity.startActivity(intent);
            activity.finish();
        });
    }

    // Dismiss -------------------------------------------------------------------------------------
    public static void dismissDialog() {
        if (dialog == null){
            return;
        }

        Runnable dismissTask = () -> {
            try {
                if (!dialog.isShowing()) return;

                Context context = dialog.getContext();

                // Safety check: prevent dismiss on destroyed/finishing activity
                if (context instanceof Activity activity && (activity.isFinishing() || activity.isDestroyed())) {
                    dialog = null;
                    return;
                }

                dialog.dismiss();
            } catch (IllegalArgumentException | IllegalStateException ignored) {
                // WindowManager no longer has this view; safe to ignore
            } finally {
                dialog = null;
            }
        };

        // Ensure the dismiss runs on main thread
        if (Looper.myLooper() == Looper.getMainLooper()) {
            dismissTask.run();
        } else {
            new Handler(Looper.getMainLooper()).post(dismissTask);
        }
    }



    // isShowing -----------------------------------------------------------------------------------
    public boolean isShowing() {
        return dialog != null && dialog.isShowing();
    }

    // Listener ------------------------------------------------------------------------------------
    public interface CancelListener {
        void onCancel();
    }

    public interface LogoutListener {
        void onLogout();
    }

    public interface DeleteListener {
        void onDelete();
    }

    public interface DownloadListener {
        void onDownload(String type);
    }

    public interface LiveDownloadListener {
        void onDownload();
    }

    public interface ScreenDialogListener {
        void onSubmit(int screen);
        void onCancel();
    }

    public interface RadioBtnListener {
        void onSetLimit(int update);
    }

    public interface ChildCountListener {
        void onUnLock(int pos);
    }

    public interface FilterDialogListener {
        void onSubmit();
    }
}