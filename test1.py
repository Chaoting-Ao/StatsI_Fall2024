import random

def get_difficulty():
    while True:
        level = input("选择难度 (1-简单, 2-中等, 3-困难): ")
        if level in ['1', '2', '3']:
            return int(level)
        print("无效输入,请重试。")

def play_game(max_number, max_guesses):
    secret_number = random.randint(1, max_number)
    print(f"我已经想好了一个1到{max_number}之间的数字。")
    print(f"你有{max_guesses}次机会来猜这个数字。")

    for guess_count in range(1, max_guesses + 1):
        guess = input(f"第{guess_count}次猜测,请猜一个数字: ")
        
        if not guess.isdigit():
            print("请输入一个有效的数字!")
            continue
        
        guess = int(guess)
        
        if guess < secret_number:
            print("太小了!再猜大一点。")
        elif guess > secret_number:
            print("太大了!再猜小一点。")
        else:
            print(f"恭喜你,猜对了!答案就是 {secret_number}。")
            return max_guesses - guess_count + 1  # 返回剩余的猜测次数作为分数
    
    print(f"很遗憾,你没有猜出正确答案。正确答案是 {secret_number}。")
    return 0

def main():
    print("欢迎来到猜数字游戏!")
    total_score = 0
    rounds = 0

    while True:
        difficulty = get_difficulty()
        if difficulty == 1:
            max_number, max_guesses = 50, 10
        elif difficulty == 2:
            max_number, max_guesses = 100, 7
        else:
            max_number, max_guesses = 200, 5

        score = play_game(max_number, max_guesses)
        total_score += score
        rounds += 1

        print(f"本轮得分: {score}")
        print(f"总得分: {total_score}")
        print(f"平均得分: {total_score / rounds:.2f}")

        play_again = input("是否再玩一轮? (y/n): ")
        if play_again.lower() != 'y':
            break

    print("游戏结束,谢谢参与!")
    print(f"总共玩了 {rounds} 轮")
    print(f"总得分: {total_score}")
    print(f"平均得分: {total_score / rounds:.2f}")

if __name__ == "__main__":
    main()
