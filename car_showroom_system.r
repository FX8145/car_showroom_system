# Load necessary library
library(ggplot2)

# Initialize an empty dataframe for the showroom inventory
showroom_inventory <- data.frame(car_name = character(), quantity = numeric(), stringsAsFactors = FALSE)

# Define functions
add_car <- function(car_name, quantity) 
  {
  if(car_name %in% showroom_inventory$car_name) 
    {
    showroom_inventory$quantity[showroom_inventory$car_name == car_name] <<- showroom_inventory$quantity[showroom_inventory$car_name == car_name] + quantity
  } 
  else 
    {
    showroom_inventory <<- rbind(showroom_inventory, data.frame(car_name = car_name, quantity = quantity))
  }
  cat(quantity, car_name, "have been added to the inventory.\n")
}

remove_car <- function(car_name, quantity) 
  {
  if(car_name %in% showroom_inventory$car_name) 
    {
    existing_quantity <- showroom_inventory$quantity[showroom_inventory$car_name == car_name]
    if(existing_quantity > quantity)
    {
      showroom_inventory$quantity[showroom_inventory$car_name == car_name] <<- existing_quantity - quantity
      cat(quantity, car_name, "have been removed from the inventory.\n")
    } 
    else if(existing_quantity == quantity) 
    {
      showroom_inventory <<- showroom_inventory[showroom_inventory$car_name != car_name, ]
      cat("All", car_name, "have been removed from the inventory.\n")
    } 
    else 
    {
      cat("You are trying to remove more", car_name, "than available in the inventory.\n")
    }
  } 
  else 
  {
    cat(car_name, "is not available in the inventory.\n")
  }
}

display_inventory <- function() 
  {
  if (nrow(showroom_inventory) == 0) 
  {
    cat("The inventory is currently empty.\n")
  } 
  else 
  {
    cat("Current Inventory:\n")
    print(showroom_inventory)
  }
}

visualize_inventory <- function() 
  {
  if (nrow(showroom_inventory) == 0) 
  {
    cat("The inventory is empty. No visualization is available.\n")
  } 
  else 
  {
    ggplot(showroom_inventory, aes(x = car_name, y = quantity, fill = car_name)) + geom_bar(stat = "identity") + labs(title = "Car Showroom Inventory", x = "Car Name", y = "Number of Cars Available") +
      theme_minimal()
  }
}

add_car("Toyota", 2)
add_car("Honda", 4)
add_car("Audi", 3) 
add_car("BMW", 6) 
add_car("Suzuki", 5)
add_car("Hyundai", 9)
add_car("Skoda", 4)
add_car("volkswagen", 3) 
add_car("Benz", 7) 
display_inventory() 
visualize_inventory()
