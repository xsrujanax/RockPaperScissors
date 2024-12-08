import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;
import java.util.concurrent.CountDownLatch;

public class Game implements Runnable {
    private List<Player> players;
    private int gameId = 1;
    private final Object lock = new Object();
    private final CountDownLatch latch;
    private final List<Thread> playerThreads = new ArrayList<>();

    public Game(List<Player> players, CountDownLatch latch) {
        this.players = players;
        this.latch = latch;
    }

    public void startTournament() {
        System.out.println("** Rock, Paper Scissors World Championship **");
        System.out.println("Starting game log...");
        for (Player player : players) {
            Thread playerThread = new Thread(() -> {
                while (!player.isDisqualified()) {
                    Player opponent;
                    int currentGameId;
                    synchronized (lock) {
                        opponent = getRandomValidOpponent(player);
                        if (opponent == null) {
                            break;
                        }
                        currentGameId = gameId;
                        gameId++;
                        System.out.println("+ [" + currentGameId + "] new game for " + player.getName() + " -> " + opponent.getName());
                    }

                    String move1 = player.getRandomMove();
                    String move2 = opponent.getRandomMove();

                    StringBuilder gameResult = new StringBuilder();
                    gameResult.append("$ (").append(currentGameId).append(") ").append(player.getName()).append(":").append(move1)
                            .append(" -> ").append(opponent.getName()).append(":").append(move2);

                    if (move1.equals(move2)) {
                        gameResult.append(" = tie!");
                        while (move1.equals(move2)) {
                            move1 = player.getRandomMove();
                            move2 = opponent.getRandomMove();
                            gameResult.append(" $ (").append(currentGameId).append(") ").append(player.getName()).append(":").append(move1)
                                    .append(" -> ").append(opponent.getName()).append(":").append(move2);
                        }
                    }

                    synchronized (lock) {
                        if (isPlayerWin(move1, move2)) {
                            gameResult.append(" = ").append(opponent.getName()).append(" loses [")
                                    .append(Math.max(opponent.getCredits() - 1, 0)).append(" credits left]");
                            opponent.decreaseCredits();
                            if (opponent.getCredits() == 0) {
                                opponent.setDisqualified(true);
                                int index = gameResult.indexOf("$");
                                gameResult.replace(index, index + 1, "-");
                                System.out.println(gameResult.toString());
                                break;
                            }
                        } else {
                            gameResult.append(" = ").append(player.getName()).append(" loses [")
                                    .append(Math.max(player.getCredits() - 1, 0)).append(" credits left]");
                            player.decreaseCredits();
                            if (player.getCredits() == 0) {
                                player.setDisqualified(true);
                                int index = gameResult.indexOf("$");
                                gameResult.replace(index, index + 1, "-");
                                System.out.println(gameResult.toString());
                                break;
                            }
                        }
                    }

                    System.out.println(gameResult.toString());
                }
            });
            playerThreads.add(playerThread);
            playerThread.start();
        }

        for (Thread thread : playerThreads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
            }
        }

        System.out.println("\nWe have a winner...");
        printTournamentReport();
        latch.countDown();
    }

    private Player getRandomValidOpponent(Player excludePlayer) {
        List<Player> validOpponents = new ArrayList<>();
        for (Player player : players) {
            if (!player.equals(excludePlayer) && !player.isDisqualified()) {
                validOpponents.add(player);
            }
        }
        if (validOpponents.isEmpty()) {
            return null;
        }
        Random random = new Random();
        return validOpponents.get(random.nextInt(validOpponents.size()));
    }

    private synchronized boolean isPlayerWin(String move1, String move2) {
        return (move1.equals("rock") && move2.equals("scissors"))
                || (move1.equals("scissors") && move2.equals("paper"))
                || (move1.equals("paper") && move2.equals("rock"));
    }

    private synchronized void printTournamentReport() {
        System.out.println("** Tournament Report **");
        System.out.println("Players:");
        for (Player player : players) {
            System.out.println(player.getName() + ": credits used: " + player.getCreditsUsed() +
                    ", credits remaining: " + Math.max(player.getCredits(), 0));
        }
        System.out.println("-----");
        System.out.println("Total games: " + (gameId - 1));
        if (!players.isEmpty()) {
            System.out.println("winner: " + fetchWinner(players));
        }
        System.out.println("See you next year...");
    }
    private String fetchWinner(List<Player> players){
        String winner="";
        int maxCredits=0;
        for(Player player: players){
            if(player.getCredits()>maxCredits){
                winner=player.getName();
                maxCredits=player.getCredits();
            }
        }
        return winner;
    }

    @Override
    public void run() {
        startTournament();
    }

    public static void main(String[] args) {
        if (args.length < 1) {
            System.out.println("Please provide the player data file as an argument.");
            return;
        }

        List<Player> players = new ArrayList<>();
        String fileName = args[0];

        try (BufferedReader br = new BufferedReader(new FileReader(fileName))) {
            String line;
            while ((line = br.readLine()) != null) {
                line = line.trim();
                if (line.startsWith("{") && line.endsWith(".")) {
                    line = line.substring(1, line.length() - 2);
                    String[] parts = line.split(",");
                    if (parts.length == 2) {
                        String name = parts[0].trim();
                        int credits = Integer.parseInt(parts[1].trim());
                        players.add(new Player(name, credits));
                    }
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        if (players.isEmpty()) {
            System.out.println("No players found in the file.");
            return;
        }

        CountDownLatch latch = new CountDownLatch(1);
        Game game = new Game(players, latch);
        Thread masterThread = new Thread(game);
        masterThread.start();

        try {
            latch.await();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
