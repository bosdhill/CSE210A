package com.p2p.chat
import android.app.AlertDialog
import android.content.Context

class SingletonDialog(context: Context?, title: String, body: String) : AlertDialog(context) {
    fun showDialog() {
        super.show()
    }

    fun dismissDialog(): SingletonDialog? {
        super.dismiss()
        return null
    }
}