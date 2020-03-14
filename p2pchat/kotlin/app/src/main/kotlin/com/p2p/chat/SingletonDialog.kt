package com.p2p.chat
import android.app.AlertDialog
import android.content.Context
import android.content.DialogInterface
import android.util.Log

class SingletonDialog {
    private var TAG = SingletonDialog::class.simpleName
    private var dialog : AlertDialog? = null

    fun show(context: Context, listener: DialogInterface.OnClickListener?, title: String, body: String) {
        dialog?.let {
            dialog!!.dismiss()
            dialog = null
        }
        Log.i(TAG, "show")
        val alert = AlertDialog.Builder(context)
                .setTitle(title)
                .setMessage(body)
                .setPositiveButton(android.R.string.yes, listener)
                .setNegativeButton(android.R.string.no, null)
                .setIcon(android.R.drawable.ic_dialog_alert)
        dialog = alert.create()
        dialog!!.show()
    }
}