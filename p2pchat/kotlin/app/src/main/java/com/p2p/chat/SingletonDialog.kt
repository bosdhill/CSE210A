package com.p2p.chat
import android.app.AlertDialog
import android.content.Context
import android.content.DialogInterface
import android.util.Log

class SingletonDialog {
    private var TAG = SingletonDialog::class.simpleName
    private var dialog : AlertDialog? = null

    fun show(context: Context, onClickListener: DialogInterface.OnClickListener?, title: String, body: String) {
        dialog?.let {
            dialog!!.dismiss()
            dialog = null
        }
        Log.i(TAG, "show")
        val alert = AlertDialog.Builder(context)
                .setTitle(title)
                .setMessage(body)
                .setPositiveButton(android.R.string.yes) { dialog, i ->
                    onClickListener?.let {
                        onClickListener.onClick(dialog, i)
                    }
                }
                .setNegativeButton(android.R.string.no) { dialogInterface, i ->
                    Log.i(TAG, String.format("[%s] dialog no clicked", title))
                }
                .setIcon(android.R.drawable.ic_dialog_alert)
        dialog = alert.create()
        dialog!!.show()
    }
}