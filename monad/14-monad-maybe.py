class MyMaybe:
    pass

class MyJust(MyMaybe):
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return f"MyJust({self.value})"

class MyNothing(MyMaybe):
    def __str__(self):
        return "MyNothing"

class Product:
    def __init__(self, product_uid, amount, price):
        self.product_uid = product_uid
        self.amount = amount
        self.price = price

    def __str__(self):
        return f"Product(uid={self.product_uid}, amount={self.amount}, price={self.price})"

class User:
    def __init__(self, name, balance):
        self.name = name
        self.balance = balance

    def __str__(self):
        return f"User(name={self.name}, balance={self.balance})"

def get_product(uid):
    if uid == 105:
        return MyJust(Product(product_uid=105, amount=3, price=9.90))
    if uid == 107:
        return MyJust(Product(product_uid=107, amount=7, price=4.30))
    return MyNothing()

def bind(maybe, f):
    if isinstance(maybe, MyNothing):
        return MyNothing()
    if isinstance(maybe, MyJust):
        return f(maybe.value)
    assert False

def get_price(uid):
    return bind(get_product(uid), lambda product: MyJust(product.price))

def check_amount(amount):
    return MyJust(None) if amount > 0 else MyNothing()

def limit_to_stock(product_id, requested_amount):
    return bind(
        get_product(product_id),
        lambda product: MyJust(min(product.amount, requested_amount)) if product.amount > 0 else MyNothing()
    )

def debet(user, amount):
    if user.balance >= amount:
        user.balance -= amount
        return MyJust(user)
    return MyNothing()

def buy_maximum(user, product_id, amount):
    return bind(
        check_amount(amount), lambda _: bind(
            limit_to_stock(product_id, amount), lambda left: bind(
                get_price(product_id), lambda price: bind(
                    check_amount(left), lambda _:
                        debet(user, left * price)
        )   )   )   )

if __name__ == "__main__":
    print("Testing get_price function:")
    print(f"Price of product 105: {get_price(105)}")
    print(f"Price of product 106: {get_price(106)}")
    print(f"Price of product 107: {get_price(107)}")
    print()

    print("Testing limit_to_stock function:")
    print(f"Request 2 units of product 105: {limit_to_stock(105, 2)}")
    print(f"Request 5 units of product 105: {limit_to_stock(105, 5)}")
    print(f"Request 3 units of product 106: {limit_to_stock(106, 3)}")
    print()

    print("Testing buy_maximum function:")

    alice = User("Alice", 50.0)
    print(f"Initial user state: {alice}")

    result1 = buy_maximum(alice, 105, 2)
    print(f"Result of buying 2 units of product 105: {result1}")
    print(f"User state after purchase: {alice}")

    result2 = buy_maximum(alice, 107, 10)
    print(f"Result of buying 10 units of product 107: {result2}")
    print(f"User state after purchase: {alice}")

    result3 = buy_maximum(alice, 106, 1)
    print(f"Result of buying product 106: {result3}")
    print(f"User state after purchase: {alice}")

    bob = User("Bob", 10.0)
    print(f"\nInitial user state: {bob}")
    result4 = buy_maximum(bob, 105, 3)
    print(f"Result of buying 3 units of product 105: {result4}")
    print(f"User state after purchase: {bob}")
