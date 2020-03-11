//
//  ViewController.swift
//  ScaledroneChatTest
//
//  Created by Marin Benčević on 08/09/2018.
//  Copyright © 2018 Scaledrone. All rights reserved.
//

import UIKit
import MessageKit
import Hype
import Scaledrone

class ViewController: MessagesViewController, HYPStateObserver, HYPNetworkObserver, HYPMessageObserver  {
    
  var chatService: ChatService!
  var messages: [Message] = []
  var member: Member!
  var communicator: Communicator!
  var resolvedInstance: HYPInstance!
  var dialog: UIAlertController!
  var displaying: Bool!
  private var instanceSearchView: UIView!
  
  override func viewDidLoad() {
    super.viewDidLoad()
    member = Member(name: .randomName, color: .random)
    messagesCollectionView.messagesDataSource = self
    messagesCollectionView.messagesLayoutDelegate = self
    messageInputBar.delegate = self
    messagesCollectionView.messagesDisplayDelegate = self
    displaying = false
    requestHypeToStart()
    
    chatService = ChatService(member: member, onRecievedMessage: {
      [weak self] message in
      self?.messages.append(message)
      self?.messagesCollectionView.reloadData()
      self?.messagesCollectionView.scrollToBottom(animated: true)
    })
  }
    
    override func viewDidAppear(_ animated: Bool) {
        super.viewDidAppear(animated)
    }
    
    // this should be only ONE reference since only one dialog open at a time
    func showSearchDialog(title: String, message: String) {
        if displaying == false {
            NSLog("showSearchDialog")
            dialog = UIAlertController(title: title, message: message, preferredStyle: .alert)
            present(dialog, animated: true, completion: nil)
            displaying = true
        }
    }
    
    func showMessageSendingDialog() {
        if displaying == false {
            NSLog("showMessageSendingDialog")
            dialog = UIAlertController(title: "Hype sending", message: "Sending message to instance...", preferredStyle: .alert)
            present(dialog, animated: true, completion: nil)
            displaying = true
        }
    }
    
    func showMessageFailedSendingDialog() {
        if displaying == false {
            NSLog("showMessageFailedSendingDialog")
            dialog = UIAlertController(title: "Hype failed sending", message: "Sending message to instance failed", preferredStyle: .alert)
            dialog.addAction(UIAlertAction(title: "OK", style: .cancel, handler: nil))
            present(dialog, animated: true, completion: nil)
            displaying = true
        }
    }
    
    // passing closure as parameter
    func dismissDialog() {
        if displaying == true {
            NSLog("dismissDialog")
            if resolvedInstance == nil {
                NSLog("resolvedInstance is nil")
            }
            dialog.dismiss(animated: true, completion: nil)
            displaying = false
        }
    }
    
    // example of currying in Swift
    func handleConfirmPressed(instance: HYPInstance!) -> (_ alertAction: UIAlertAction) -> () {
        return {_ in
            self.resolvedInstance = instance
            NSLog("Hype will communicate with instance %@", instance.appStringIdentifier!)
        }
    }
    
    func showDidResolveDialog(title: String, message: String, instance: HYPInstance!) {
        if displaying == false {
            NSLog("showDidResolveDialog")
            dialog = UIAlertController(title: title, message: message, preferredStyle: .alert)
            dialog.addAction(UIAlertAction(title: "OK", style: .default, handler: handleConfirmPressed(instance: instance)))
            dialog.addAction(UIAlertAction(title: "CANCEL", style: .cancel, handler: nil))
            present(dialog, animated: true, completion: nil)
            displaying = true
        }
    }
    
    
    func requestHypeToStart() {
        HYP.add(self as HYPStateObserver)
        HYP.add(self as HYPNetworkObserver)
        HYP.add(self as HYPMessageObserver)
        HYP.setAppIdentifier("c990ae8f")
        HYP.start()
    }

    func hypeDidStart() {
        NSLog("Hype started!")
        DispatchQueue.main.sync {
            self.showSearchDialog(title: "Hype started", message: "Searching for instances...")
        }
    }

    func hypeDidStopWithError(_ error: HYPError!) {
        let description:String! = error == nil ? "" : error.description
        NSLog("Hype stopped [%@]", description)
    }

    func hypeDidFailStartingWithError(_ error: HYPError!) {
        NSLog("Hype failed starting [%@]", error.description)
        NSLog("Hype code [%d]", error.code.rawValue)
        NSLog("Hype suggestion [%@]", error.suggestion)
    }

    func hypeDidChangeState() {
        NSLog("Hype state changed to [%d] (Idle=0, Starting=1, Running=2, Stopping=3)", HYP.state().rawValue)
    }

    func hypeDidBecomeReady() {
        NSLog("Hype is ready")
        requestHypeToStart()
    }

    func hypeDidRequestAccessToken(withUserIdentifier userIdentifier: UInt) -> String! {
        return "3905669394fa2533"
    }

    func hypeDidFind(_ instance: HYPInstance!) {
        NSLog("Hype did find instance %@", instance.appStringIdentifier!)
    }

    func hypeDidLose(_ instance: HYPInstance!, error: HYPError!) {
        NSLog("Hype did lose instance %@ [%s]", instance.appStringIdentifier!, error.description)
        DispatchQueue.main.async {
            self.showSearchDialog(title: "Hype instance lost", message: "Searching for instances...")
        }
    }

    func hypeDidResolve(_ instance: HYPInstance!) {
        NSLog("Hype resolved instance: %@", instance.stringIdentifier!)
        DispatchQueue.main.async {
            // example of closure in Swift
            // self contained block of code executed on completion
            self.dismissDialog()
            self.showDidResolveDialog(title: "Hype instance found", message: String(format: "Instance found: %@\nDo you wish to communicate?", instance.stringIdentifier), instance: instance)
        }
    }

    func hypeDidFailResolving(_ instance: HYPInstance!, error: HYPError!) {
        NSLog("Hype did fail resolving instance %@ [%s]", instance.appStringIdentifier!, error.description)
    }

    func hypeDidReceive(_ message: HYPMessage!, from fromInstance: HYPInstance!) {
        NSLog("Hype did receive %d %@", message.info.identifier, fromInstance.appStringIdentifier!)
        let msg = (NSString(data: (message?.data)!, encoding: String.Encoding.utf8.rawValue)! as String)
        NSLog("Hype msg recieved [%@]", msg)
        DispatchQueue.main.sync {
            self.messages.append(Message(member: Member(name: "other", color: UIColor(displayP3Red: 1.0, green: 1.0, blue: 0.0, alpha: 1.0)), text: msg, messageId: "32"))
            self.messagesCollectionView.reloadData()
            self.messagesCollectionView.scrollToBottom(animated: true)
        }
    }

    func hypeDidFailSendingMessage(_ messageInfo: HYPMessageInfo!, to toInstance: HYPInstance!, error: HYPError!) {
        NSLog("Hype did fail sending  %d %@ %s", messageInfo.identifier, toInstance.appStringIdentifier!, error.description)
    }
    
    func hypeDidDeliverMessage(_ messageInfo: HYPMessageInfo!, to toInstance: HYPInstance!, progress: Float, complete: Bool) {
        NSLog("Hype did successfully deliver  %d %@ %f %b", messageInfo.identifier, toInstance.appStringIdentifier!, progress, complete)
        DispatchQueue.main.sync {
            self.dismissDialog()
        }
    }

}

extension ViewController: MessagesDataSource {
  func numberOfSections(in messagesCollectionView: MessagesCollectionView) -> Int {
    return messages.count
  }
  
  func currentSender() -> Sender {
    return Sender(id: member.name, displayName: member.name)
  }
  
  func messageForItem(at indexPath: IndexPath,
                      in messagesCollectionView: MessagesCollectionView) -> MessageType {
    
    return messages[indexPath.section]
  }
  
  func messageTopLabelHeight(for message: MessageType, at indexPath: IndexPath, in messagesCollectionView: MessagesCollectionView) -> CGFloat {
    return 12
  }
  
  func messageTopLabelAttributedText(for message: MessageType, at indexPath: IndexPath) -> NSAttributedString? {
    return NSAttributedString(
      string: message.sender.displayName,
      attributes: [.font: UIFont.systemFont(ofSize: 12)])
  }
}

extension ViewController: MessagesLayoutDelegate {
  func heightForLocation(message: MessageType,
                         at indexPath: IndexPath,
                         with maxWidth: CGFloat,
                         in messagesCollectionView: MessagesCollectionView) -> CGFloat {
    return 0
  }
}

extension ViewController: MessagesDisplayDelegate {
  func configureAvatarView(
    _ avatarView: AvatarView,
    for message: MessageType,
    at indexPath: IndexPath,
    in messagesCollectionView: MessagesCollectionView) {
    
    let message = messages[indexPath.section]
    let color = message.member.color
    avatarView.backgroundColor = color
  }
}

extension ViewController: MessageInputBarDelegate {
  func messageInputBar(_ inputBar: MessageInputBar, didPressSendButtonWith text: String) {
    sendMessage(text: text)
    inputBar.inputTextView.text = ""
  }
    func sendMessage(text: String) {
        let col = UIColor(displayP3Red: 1.0, green: 0.0, blue: 0.0, alpha: 1.0)
        let m = Member(name: "Bobby", color: col)
        let chatMessage = Message(
            member: m,
            text: text,
            messageId: UUID().uuidString)
        
        let data: Data? = text.data(using: String.Encoding.utf8)
        
        if self.resolvedInstance != nil {
            let _: HYPMessage? = HYP.send(data, to: self.resolvedInstance)
            messageCallback(message: chatMessage)
        }
        else {
            DispatchQueue.main.sync {
                self.showMessageFailedSendingDialog()
            }
        }
    }
    
    func messageCallback(message: Message) {
        DispatchQueue.main.async {
            self.showMessageSendingDialog()
            self.messages.append(message)
            self.messagesCollectionView.reloadData()
            self.messagesCollectionView.scrollToBottom(animated: true)
        }
    }
}

