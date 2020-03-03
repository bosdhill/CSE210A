package com.example.scaledrone.chat.widgets;

import android.content.DialogInterface;

import com.hypelabs.hype.Instance;

public class ChatDialogOnClickListener implements DialogInterface.OnClickListener {

    Instance instance;
    Instance receiver;
    public ChatDialogOnClickListener(Instance instance, Instance receiver) {
        this.instance = instance;
        this.receiver = receiver;
    }

    @Override
    public void onClick(DialogInterface dialogInterface, int i) {
        this.instance = this.receiver;
    }
}
