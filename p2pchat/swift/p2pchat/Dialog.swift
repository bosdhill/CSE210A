import UIKit

class Dialog: NSObject {
    var dialog: UIAlertController!
    
    func show(title: String, message: String, handler: ((UIAlertAction) -> Void)?) -> UIAlertController {
        if dialog != nil {
            dialog.dismiss(animated: true, completion: nil)
            dialog = nil
        }
        NSLog("show")
        dialog = UIAlertController(title: title, message: message, preferredStyle: .alert)
        dialog.addAction(UIAlertAction(title: "OK", style: .default, handler: handler))
        dialog.addAction(UIAlertAction(title: "CANCEL", style: .cancel, handler: nil))
        return dialog
    }
}
