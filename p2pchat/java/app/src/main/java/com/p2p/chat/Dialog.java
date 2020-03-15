package com.p2p.chat;
import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.util.Log;

public class Dialog {
    private String TAG = Dialog.class.getName();
    private AlertDialog dialog;

    public void show(Context context, DialogInterface.OnClickListener listener, String title, String body) {
        if (dialog != null) {
            dialog.dismiss();
            dialog = null;
        }
        Log.i(TAG, "show");
        AlertDialog.Builder alert = new AlertDialog.Builder(context)
                .setTitle(title)
                .setMessage(body)
                .setIcon(android.R.drawable.ic_dialog_alert)
                .setPositiveButton(android.R.string.yes, listener)
                .setNegativeButton(android.R.string.no, null);
        dialog = alert.create();
        dialog.show();
    }
}
