package com.p2p.chat
import android.app.AlertDialog
import android.content.Context
import android.content.DialogInterface
import android.util.Log

class Dialog {
    private var TAG = Dialog::class.simpleName
    private var dialog : AlertDialog? = null

    fun show(context: Context, listener: DialogInterface.OnClickListener?, title: String, body: String, canceleable: Boolean) {
        dialog?.let {
            dialog!!.dismiss()
            dialog = null
        }
        Log.i(TAG, "show")
        val alert = AlertDialog.Builder(context)
                .setTitle(title)
                .setMessage(body)
                .setIcon(android.R.drawable.ic_dialog_alert)
                .setCancelable(canceleable)
        listener?.let {
            alert.setPositiveButton(android.R.string.yes, listener)
                 .setNegativeButton(android.R.string.no, null)
        }
        if (canceleable) {
            alert.setPositiveButton(android.R.string.yes, null)
        }
        dialog = alert.create()
        dialog!!.show()
    }
}