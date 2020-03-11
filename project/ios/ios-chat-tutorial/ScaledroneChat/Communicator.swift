//
//  Communicator.swift
//  ScaledroneChat
//
//  Created by Bobby Dhillon on 2/29/20.
//  Copyright © 2020 Scaledrone. All rights reserved.
//

import Hype

class Communicator: NSObject, HYPStateObserver, HYPNetworkObserver, HYPMessageObserver {
    
    var announcement: String = "New user found"
    
    func requestHypeToStart() {
        
        // Add self as an Hype observer
        HYP.add(self as HYPStateObserver)
        HYP.add(self as HYPNetworkObserver)
        HYP.add(self as HYPMessageObserver)
        
        HYP.setAnnouncement(self.announcement.data(using: .utf8))
        
        // Generate an app identifier in the HypeLabs dashboard (https://hypelabs.io/apps/),
        // by creating a new app. Copy the given identifier here.
        HYP.setAppIdentifier("c990ae8f")
        
        HYP.start()
        
//        // Update the text label
//        self.updateHypeInstancesLabel()
    }
    
    func hypeDidStart() {
        NSLog("Hype started!")
    }
    
    func hypeDidStopWithError(_ error: HYPError!) {
        let description:String! = error == nil ? "" : error.description
        NSLog("Hype stopped [%@]", description)
    }
    
    func hypeDidFailStartingWithError(_ error: HYPError!) {
        NSLog("Hype failed starting [%@]", error.description)
        NSLog("Hype code [%d]", error.code.rawValue)
        NSLog("Hype suggestion [%@]", error.suggestion)
        
        // return alert
//        let errorMessage : String = "Description: " + (error.description as String) + "\nReason:" + (error.reason as String)  + "\nSuggestion:" + (error.suggestion as String)
//        let alert = UIAlertController(title: "Hype failed starting", message: errorMessage, preferredStyle: UIAlertControllerStyle.alert)
//        alert.addAction(UIAlertAction(title: "Ok", style: UIAlertActionStyle.default, handler: nil))
//        self.present(alert, animated: true, completion: nil)
    }
    
    func hypeDidChangeState()
    {
        NSLog("Hype state changed to [%d] (Idle=0, Starting=1, Running=2, Stopping=3)", HYP.state().rawValue)
    }
    
    func hypeDidBecomeReady() {
        NSLog("Hype is ready")
        
        // Where're here due to a failed start request, try again
        requestHypeToStart()
    }
    
    func hypeDidRequestAccessToken(withUserIdentifier userIdentifier: UInt) -> String! {
        return "3905669394fa2533"
    }
    
    func hypeDidFind(_ instance: HYPInstance!) {
        NSLog("Hype did find instance %@", instance.appStringIdentifier!)
    }
    
    func hypeDidLose(_ instance: HYPInstance!, error: HYPError!) {
        NSLog("Hype did lost instance %@ [%s]", instance.appStringIdentifier!, error.description)
    }
    
    func hypeDidResolve(_ instance: HYPInstance!)
    {
        NSLog("Hype resolved instance: %@", instance.stringIdentifier!)
        
//        // This device is now capable of communicating
//        addToResolvedInstancesDict(instance)
    }
    
    func hypeDidFailResolving(_ instance: HYPInstance!, error: HYPError!) {
        NSLog("Hype did fail resolving instance %@ [%s]", instance.appStringIdentifier!, error.description)
    }
    
    func hypeDidReceive(_ message: HYPMessage!, from fromInstance: HYPInstance!) {
        NSLog("Hype did receive %d %@", message.info.identifier, fromInstance.appStringIdentifier!)
    }
    
    func hypeDidFailSendingMessage(_ messageInfo: HYPMessageInfo!, to toInstance: HYPInstance!, error: HYPError!) {
        NSLog("Hype did fail sending  %d %@ %s", messageInfo.identifier, toInstance.appStringIdentifier!, error.description)
    }    
    
}
