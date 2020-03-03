package com.example.scaledrone.chat.widgets;

import android.app.AlertDialog;
import android.content.Context;
import android.content.DialogInterface;
import android.util.Log;

import com.example.scaledrone.chat.MainActivity;

public class ChatDialog extends AlertDialog implements Runnable {

    private static String TAG = ChatDialog.class.getName();

    protected ChatDialog(Context context) {
        super(context);
    }

    static AlertDialog resolveDialogBuilder(Context context, String message) {
        final AlertDialog.Builder confirm = new AlertDialog.Builder(context)
                .setTitle("New Instance Resolved")
                .setMessage(String.format("Instance found: %s\nDo you wish to communicate?", message))
                .setPositiveButton(android.R.string.yes, new DialogInterface.OnClickListener() {
                    public void onClick(DialogInterface dialog, int which) {
                        Log.d(TAG, "Hype will communicate with instance");
//                        setResolveDialogIsOpen(false);
                    }
                })
                .setNegativeButton(android.R.string.no, new DialogInterface.OnClickListener() {
                    @Override
                    public void onClick(DialogInterface dialogInterface, int i) {
//                        setResolveDialogIsOpen(false);
                    }
                })
                .setIcon(android.R.drawable.ic_dialog_alert);
        return confirm.create();
    }

    @Override
    public void run() {

    }
}
