import Array "mo:base/Array";
import Buffer "mo:base/Buffer";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Option "mo:base/Option";
import Principal "mo:base/Principal";
import Result "mo:base/Result";
import Text "mo:base/Text";
import Time "mo:base/Time";

actor AntiqueStore {
  
  // Types for the store
  type ItemId = Nat;
  type UserId = Principal;
  
  // Item structure for antiques
  type Item = {
    id: ItemId;
    name: Text;
    description: Text;
    price: Nat; // Price in ICP tokens (with 8 decimal places, e.g., 1.00000000 ICP = 100000000)
    imageUrls: [Text];
    category: Text;
    condition: Text;
    dateAdded: Time.Time;
    seller: UserId;
    available: Bool;
    dimensions: ?Text; // Optional dimensions
    weight: ?Nat; // Optional weight
    era: ?Text; // Optional era/period
  };
  
  // Order status types
  type OrderStatus = {
    #pending;
    #paid;
    #shipped;
    #delivered;
    #cancelled;
  };
  
  // Order structure for purchases
  type Order = {
    id: Nat;
    buyer: UserId;
    items: [ItemId];
    totalPrice: Nat;
    status: OrderStatus;
    createdAt: Time.Time;
    updatedAt: Time.Time;
    shippingAddress: Text;
  };
  
  // User profile structure
  type UserProfile = {
    id: UserId;
    username: Text;
    email: ?Text;
    bio: ?Text;
    isSeller: Bool;
    createdAt: Time.Time;
  };
  
  // Review structure
  type Review = {
    reviewer: UserId;
    itemId: ItemId;
    rating: Nat; // 1-5 stars
    comment: Text;
    timestamp: Time.Time;
  };
  
  // Stable variables that persist across canister upgrades
  private stable var nextItemId: Nat = 1;
  private stable var nextOrderId: Nat = 1;
  private stable var itemEntries : [(ItemId, Item)] = [];
  private stable var orderEntries : [(Nat, Order)] = [];
  private stable var userProfileEntries : [(Principal, UserProfile)] = [];
  private stable var reviewEntries : [(ItemId, [Review])] = [];
  
  // Runtime hashmap data structures (recreated on canister upgrade)
  private var items = HashMap.HashMap<ItemId, Item>(0, Nat.equal, Nat.hash);
  private var orders = HashMap.HashMap<Nat, Order>(0, Nat.equal, Nat.hash);
  private var userProfiles = HashMap.HashMap<Principal, UserProfile>(0, Principal.equal, Principal.hash);
  private var reviews = HashMap.HashMap<ItemId, [Review]>(0, Nat.equal, Nat.hash);
  
  // System initialization
  system func preupgrade() {
    // Save state before upgrades
    itemEntries := Iter.toArray(items.entries());
    orderEntries := Iter.toArray(orders.entries());
    userProfileEntries := Iter.toArray(userProfiles.entries());
    reviewEntries := Iter.toArray(reviews.entries());
  };
  
  system func postupgrade() {
    // Restore state after upgrades
    items := HashMap.fromIter<ItemId, Item>(itemEntries.vals(), 10, Nat.equal, Nat.hash);
    orders := HashMap.fromIter<Nat, Order>(orderEntries.vals(), 10, Nat.equal, Nat.hash);
    userProfiles := HashMap.fromIter<Principal, UserProfile>(userProfileEntries.vals(), 10, Principal.equal, Principal.hash);
    reviews := HashMap.fromIter<ItemId, [Review]>(reviewEntries.vals(), 10, Nat.equal, Nat.hash);
    
    // Clear stable variables to free memory
    itemEntries := [];
    orderEntries := [];
    userProfileEntries := [];
    reviewEntries := [];
  };
  
  // ---------- USER MANAGEMENT FUNCTIONS ----------
  
  // Create/update user profile
  public shared(msg) func createOrUpdateProfile(username: Text, email: ?Text, bio: ?Text, isSeller: Bool) : async Result.Result<UserProfile, Text> {
    let caller = msg.caller;
    
    // Don't allow anonymous principals
    if (Principal.isAnonymous(caller)) {
      return #err("Cannot create profile for anonymous user");
    };
    
    let timestamp = Time.now();
    let profile : UserProfile = {
      id = caller;
      username = username;
      email = email;
      bio = bio;
      isSeller = isSeller;
      createdAt = switch (userProfiles.get(caller)) {
        case (?existing) { existing.createdAt };
        case (null) { timestamp };
      };
    };
    
    userProfiles.put(caller, profile);
    return #ok(profile);
  };
  
  // Get user profile
  public query func getProfile(userId: Principal) : async ?UserProfile {
    userProfiles.get(userId);
  };
  
  // Get own profile
  public shared query(msg) func getOwnProfile() : async ?UserProfile {
    userProfiles.get(msg.caller);
  };
  
  // ---------- ITEM MANAGEMENT FUNCTIONS ----------
  
  // Add a new item
  public shared(msg) func addItem(
    name: Text,
    description: Text,
    price: Nat,
    imageUrls: [Text],
    category: Text,
    condition: Text,
    dimensions: ?Text,
    weight: ?Nat,
    era: ?Text
  ) : async Result.Result<Item, Text> {
    let caller = msg.caller;
    
    // Check if user exists and is a seller
    switch (userProfiles.get(caller)) {
      case (null) { return #err("User profile not found. Please create a profile first.") };
      case (?profile) {
        if (not profile.isSeller) {
          return #err("Only sellers can add items");
        };
      };
    };
    
    let itemId = nextItemId;
    nextItemId += 1;
    
    let newItem : Item = {
      id = itemId;
      name = name;
      description = description;
      price = price;
      imageUrls = imageUrls;
      category = category;
      condition = condition;
      dimensions = dimensions;
      weight = weight;
      era = era;
      dateAdded = Time.now();
      seller = caller;
      available = true;
    };
    
    items.put(itemId, newItem);
    return #ok(newItem);
  };
  
  // Update an existing item
  public shared(msg) func updateItem(
    itemId: ItemId,
    name: ?Text,
    description: ?Text,
    price: ?Nat,
    imageUrls: ?[Text],
    category: ?Text,
    condition: ?Text,
    available: ?Bool,
    dimensions: ?Text,
    weight: ?Nat,
    era: ?Text
  ) : async Result.Result<Item, Text> {
    let caller = msg.caller;
    
    // Check if item exists
    switch (items.get(itemId)) {
      case (null) { return #err("Item not found") };
      case (?item) {
        // Check if caller is the seller
        if (item.seller != caller) {
          return #err("Only the seller can update this item");
        };
        
        // Update fields if provided
        let updatedItem : Item = {
          id = item.id;
          name = Option.get(name, item.name);
          description = Option.get(description, item.description);
          price = Option.get(price, item.price);
          imageUrls = Option.get(imageUrls, item.imageUrls);
          category = Option.get(category, item.category);
          condition = Option.get(condition, item.condition);
          dimensions = Option.get(dimensions, item.dimensions);
          weight = Option.get(weight, item.weight);
          era = Option.get(era, item.era);
          dateAdded = item.dateAdded;
          seller = item.seller;
          available = Option.get(available, item.available);
        };
        
        items.put(itemId, updatedItem);
        return #ok(updatedItem);
      };
    };
  };
  
  // Get item by ID
  public query func getItem(itemId: ItemId) : async ?Item {
    items.get(itemId);
  };
  
  // List all items
  public query func listItems() : async [Item] {
    Iter.toArray(items.vals());
  };
  
  // List items by category
  public query func listItemsByCategory(category: Text) : async [Item] {
    let filtered = Buffer.Buffer<Item>(10);
    for (item in items.vals()) {
      if (item.category == category) {
        filtered.add(item);
      };
    };
    Buffer.toArray(filtered);
  };
  
  // List items by seller
  public query func listItemsBySeller(seller: Principal) : async [Item] {
    let filtered = Buffer.Buffer<Item>(10);
    for (item in items.vals()) {
      if (Principal.equal(item.seller, seller)) {
        filtered.add(item);
      };
    };
    Buffer.toArray(filtered);
  };
  
  // Search items by name or description
  public query func searchItems(query: Text) : async [Item] {
    let searchQuery = Text.toLowercase(query);
    let results = Buffer.Buffer<Item>(10);
    
    for (item in items.vals()) {
      let nameMatch = Text.contains(Text.toLowercase(item.name), #text searchQuery);
      let descMatch = Text.contains(Text.toLowercase(item.description), #text searchQuery);
      
      if (nameMatch or descMatch) {
        results.add(item);
      };
    };
    
    Buffer.toArray(results);
  };
  
  // ---------- ORDER MANAGEMENT FUNCTIONS ----------
  
  // Create an order (simplified, no actual payment integration yet)
  public shared(msg) func createOrder(
    itemIds: [ItemId],
    shippingAddress: Text
  ) : async Result.Result<Order, Text> {
    let caller = msg.caller;
    
    // Check if user exists
    switch (userProfiles.get(caller)) {
      case (null) { return #err("User profile not found. Please create a profile first.") };
      case (_) { /* User exists, continue */ };
    };
    
    // Validate items and calculate total price
    var totalPrice : Nat = 0;
    let validItems = Buffer.Buffer<ItemId>(itemIds.size());
    
    for (itemId in itemIds.vals()) {
      switch (items.get(itemId)) {
        case (null) { return #err("Item #" # Nat.toText(itemId) # " not found") };
        case (?item) {
          if (not item.available) {
            return #err("Item #" # Nat.toText(itemId) # " is not available");
          };
          totalPrice += item.price;
          validItems.add(itemId);
        };
      };
    };
    
    let timestamp = Time.now();
    let orderId = nextOrderId;
    nextOrderId += 1;
    
    let newOrder : Order = {
      id = orderId;
      buyer = caller;
      items = Buffer.toArray(validItems);
      totalPrice = totalPrice;
      status = #pending;
      createdAt = timestamp;
      updatedAt = timestamp;
      shippingAddress = shippingAddress;
    };
    
    orders.put(orderId, newOrder);
    
    // Mark items as unavailable
    for (itemId in validItems.vals()) {
      switch (items.get(itemId)) {
        case (null) { /* Already checked existence, should not happen */ };
        case (?item) {
          let updatedItem : Item = {
            id = item.id;
            name = item.name;
            description = item.description;
            price = item.price;
            imageUrls = item.imageUrls;
            category = item.category;
            condition = item.condition;
            dimensions = item.dimensions;
            weight = item.weight;
            era = item.era;
            dateAdded = item.dateAdded;
            seller = item.seller;
            available = false;
          };
          items.put(itemId, updatedItem);
        };
      };
    };
    
    return #ok(newOrder);
  };
  
  // Update order status
  public shared(msg) func updateOrderStatus(orderId: Nat, newStatus: OrderStatus) : async Result.Result<Order, Text> {
    let caller = msg.caller;
    
    // Check if order exists
    switch (orders.get(orderId)) {
      case (null) { return #err("Order not found") };
      case (?order) {
        // Check item seller permissions
        let hasPermission = order.buyer == caller;
        
        if (not hasPermission) {
          // Check if caller is a seller of any item in the order
          for (itemId in order.items.vals()) {
            switch (items.get(itemId)) {
              case (?item) {
                if (item.seller == caller) {
                  hasPermission := true;
                  break;
                };
              };
              case (null) { /* Item not found, skip */ };
            };
          };
        };
        
        if (not hasPermission) {
          return #err("You don't have permission to update this order");
        };
        
        let updatedOrder : Order = {
          id = order.id;
          buyer = order.buyer;
          items = order.items;
          totalPrice = order.totalPrice;
          status = newStatus;
          createdAt = order.createdAt;
          updatedAt = Time.now();
          shippingAddress = order.shippingAddress;
        };
        
        orders.put(orderId, updatedOrder);
        return #ok(updatedOrder);
      };
    };
  };
  
  // Get order by ID
  public shared query(msg) func getOrder(orderId: Nat) : async Result.Result<Order, Text> {
    let caller = msg.caller;
    
    switch (orders.get(orderId)) {
      case (null) { return #err("Order not found") };
      case (?order) {
        // Check permissions (buyer or seller can view)
        let hasPermission = order.buyer == caller;
        
        if (not hasPermission) {
          // Check if caller is a seller of any item in the order
          for (itemId in order.items.vals()) {
            switch (items.get(itemId)) {
              case (?item) {
                if (item.seller == caller) {
                  hasPermission := true;
                  break;
                };
              };
              case (null) { /* Item not found, skip */ };
            };
          };
        };
        
        if (not hasPermission) {
          return #err("You don't have permission to view this order");
        };
        
        return #ok(order);
      };
    };
  };
  
  // Get orders for buyer
  public shared query(msg) func getMyOrders() : async [Order] {
    let caller = msg.caller;
    let userOrders = Buffer.Buffer<Order>(10);
    
    for (order in orders.vals()) {
      if (order.buyer == caller) {
        userOrders.add(order);
      };
    };
    
    Buffer.toArray(userOrders);
  };
  
  // Get orders for sellers
  public shared query(msg) func getOrdersForSeller() : async [Order] {
    let caller = msg.caller;
    let sellerOrders = Buffer.Buffer<Order>(10);
    
    // Collect all items for this seller
    let sellerItems = Buffer.Buffer<ItemId>(10);
    for (item in items.vals()) {
      if (item.seller == caller) {
        sellerItems.add(item.id);
      };
    };
    
    // Get all orders containing the seller's items
    let sellerItemIds = Buffer.toArray(sellerItems);
    for (order in orders.vals()) {
      for (itemId in order.items.vals()) {
        // Check if this order contains any of the seller's items
        for (sellerItemId in sellerItemIds.vals()) {
          if (itemId == sellerItemId) {
            sellerOrders.add(order);
            break;
          };
        };
      };
    };
    
    Buffer.toArray(sellerOrders);
  };
  
  // ---------- REVIEW FUNCTIONS ----------
  
  // Add a review for an item
  public shared(msg) func addReview(
    itemId: ItemId,
    rating: Nat,
    comment: Text
  ) : async Result.Result<Review, Text> {
    let caller = msg.caller;
    
    // Check if item exists
    switch (items.get(itemId)) {
      case (null) { return #err("Item not found") };
      case (_) { /* Item exists, continue */ };
    };
    
    // Validate rating (1-5 stars)
    if (rating < 1 or rating > 5) {
      return #err("Rating must be between 1 and 5");
    };
    
    // Create the review
    let newReview : Review = {
      reviewer = caller;
      itemId = itemId;
      rating = rating;
      comment = comment;
      timestamp = Time.now();
    };
    
    // Add the review to the item's reviews
    switch (reviews.get(itemId)) {
      case (null) {
        // First review for this item
        reviews.put(itemId, [newReview]);
      };
      case (?existingReviews) {
        // Check if user already left a review
        for (review in existingReviews.vals()) {
          if (review.reviewer == caller) {
            return #err("You have already reviewed this item");
          };
        };
        
        // Add new review
        let updatedReviews = Array.append(existingReviews, [newReview]);
        reviews.put(itemId, updatedReviews);
      };
    };
    
    return #ok(newReview);
  };
  
  // Get reviews for an item
  public query func getReviews(itemId: ItemId) : async [Review] {
    switch (reviews.get(itemId)) {
      case (null) { return [] };
      case (?itemReviews) { return itemReviews };
    };
  };
  
  // ---------- PUBLIC STATS/INFO FUNCTIONS ----------
  
  // Get store stats
  public query func getStoreStats() : async {
    totalItems: Nat;
    totalUsers: Nat;
    totalOrders: Nat;
  } {
    return {
      totalItems = items.size();
      totalUsers = userProfiles.size();
      totalOrders = orders.size();
    };
  };
}
