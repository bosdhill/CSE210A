//
//  ChatService.swift
//  ScaledroneChatTest
//
//  Created by Marin Benčević on 08/09/2018.
//  Copyright © 2018 Scaledrone. All rights reserved.
//

import Foundation
import Scaledrone

class ChatService {
  
  private let scaledrone: Scaledrone
  private let messageCallback: (Message)-> Void
  
  private var room: ScaledroneRoom?
  
  init(member: Member, onRecievedMessage: @escaping (Message)-> Void) {
    self.messageCallback = onRecievedMessage
//    #error("Make sure to input your channel ID and delete this line.")
    self.scaledrone = Scaledrone(
      channelID: "YOUR-CHANNEL-ID",
      data: member.toJSON)
    scaledrone.delegate = self
  }
  
  func connect() {
    scaledrone.connect()
  }
  
  func sendMessage(_ message: String) {
//    room?.publish(message: message)
    let col = UIColor(displayP3Red: 1.0, green: 0.0, blue: 0.0, alpha: 1.0)
    let m = Member(name: "Bobby", color: col)
    let message = Message(
        member: m,
        text: message,
        messageId: UUID().uuidString)
//    let data: Data? = message.data(using: String.Encoding.utf8)
    
//    let message: HYPMessage? = HYP.send(message, to: store?.instance)
    messageCallback(message)
  }
  
}

extension ChatService: ScaledroneDelegate {
  
  func scaledroneDidConnect(scaledrone: Scaledrone, error: NSError?) {
    print("Connected to Scaledrone")
    room = scaledrone.subscribe(roomName: "observable-room")
    room?.delegate = self
  }
  
  func scaledroneDidReceiveError(scaledrone: Scaledrone, error: NSError?) {
    print("Scaledrone error", error ?? "")
  }
  
  func scaledroneDidDisconnect(scaledrone: Scaledrone, error: NSError?) {
    print("Scaledrone disconnected", error ?? "")
  }
  
}

extension ChatService: ScaledroneRoomDelegate {
  
  func scaledroneRoomDidConnect(room: ScaledroneRoom, error: NSError?) {
    print("Connected to room!")
  }
  
  func scaledroneRoomDidReceiveMessage(
    room: ScaledroneRoom,
    message: Any,
    member: ScaledroneMember?) {
    
    guard
      let text = message as? String,
      let memberData = member?.clientData,
      let member = Member(fromJSON: memberData)
      else {
        print("Could not parse data.")
        return
    }
    
    let message = Message(
      member: member,
      text: text,
      messageId: UUID().uuidString)
    messageCallback(message)
  }
}

