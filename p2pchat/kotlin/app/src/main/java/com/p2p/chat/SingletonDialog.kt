package com.p2p.chat
import android.app.AlertDialog
import android.content.Context
import android.content.DialogInterface
import android.util.Log
import com.hypelabs.hype.Instance

class SingletonDialog {
    private var TAG = SingletonDialog::class.simpleName
    private var dialog : AlertDialog? = null

    fun showInstanceLostDialog(context: Context, body: String) {
        dialog?.let {
            dialog!!.dismiss()
            dialog = null
        }
        val confirm = AlertDialog.Builder(context)
                .setTitle("Hype instance lost...")
                .setMessage(body)
                .setIcon(android.R.drawable.ic_dialog_alert)
        dialog = confirm.create()
        dialog!!.show()
    }

    fun showInstanceSearchDialog(context: Context) {
        dialog?.let {
            dialog!!.dismiss()
            dialog = null
        }
        val confirm = AlertDialog.Builder(context)
                .setTitle("Hype started...")
                .setMessage("Looking for instances")
                .setIcon(android.R.drawable.ic_dialog_alert)
        dialog = confirm.create()
        dialog!!.show()
    }

    fun showSentFailedDialog(context: Context, instance: Instance) {
        dialog?.let {
            dialog!!.dismiss()
            dialog = null
        }
        val confirm = AlertDialog.Builder(context)
                .setTitle("Sending Failed")
                .setMessage(String.format("Could not send to %s", instance.stringIdentifier))
                .setPositiveButton(android.R.string.yes) { dialog, which ->
                    Log.i(TAG, "Hype sent failed dialog clicked")
                }
                .setIcon(android.R.drawable.ic_dialog_alert)
        dialog = confirm.create()
        dialog!!.show()
    }

    fun showNoInstanceDialog(context: Context, onClickListener: DialogInterface.OnClickListener) {
        dialog?.let {
            dialog!!.dismiss()
            dialog = null
        }
        val confirm = AlertDialog.Builder(context)
                .setTitle("No resolved instance")
                .setMessage(String.format("Would you like to search for an instance?"))
                .setPositiveButton(android.R.string.yes) {
                    dialog, i: Int ->
                    onClickListener.onClick(dialog, i)
                }
                .setNegativeButton(android.R.string.no) {
                    dialog, which -> Log.i(TAG, "Hype no instance no clicked")
                }
                .setIcon(android.R.drawable.ic_dialog_alert)
        dialog = confirm.create()
        dialog!!.show()
    }

    fun showResolveDialog(context: Context, instance: Instance,
                          onClickListener: DialogInterface.OnClickListener) {
        dialog?.let {
            dialog!!.dismiss()
            dialog = null
        }
        Log.i(TAG, "showResolveDialog")
        val confirm = AlertDialog.Builder(context)
                .setTitle("New Instance Resolved")
                .setMessage(String.format("Instance found: %s\nDo you wish to communicate?",
                        instance.stringIdentifier))
                .setPositiveButton(android.R.string.yes) { dialog, i ->
                    onClickListener.onClick(dialog, i)
                }
                .setNegativeButton(android.R.string.no) { dialogInterface, i -> }
                .setIcon(android.R.drawable.ic_dialog_alert)
        dialog = confirm.create()
        dialog!!.show()
        Log.i(TAG, "Hype show resolve dialog")
    }
}