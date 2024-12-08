import java.util.Random;

public class Player implements Runnable {
    private String name;
    private int credits;
    private int creditsUsed;
    private boolean disqualified;

    public Player(String name, int credits) {
        this.name = name;
        this.credits = credits;
        this.creditsUsed = 0;
        this.disqualified = false;
    }

    public String getName() {
        return name;
    }

    public int getCredits() {
        return credits;
    }

    public int getCreditsUsed() {
        return creditsUsed;
    }

    public synchronized String getRandomMove() {
        String[] moves = {"rock", "paper", "scissors"};
        Random random = new Random();
        return moves[random.nextInt(moves.length)];
    }

    public synchronized void decreaseCredits() {
        if (credits > 0) {
            credits--;
            creditsUsed++;
            if (credits == 0) {
                disqualified = true;
            }
        }
    }

    public synchronized boolean acceptInvitation() {
        return credits > 0 && !disqualified;
    }

    public synchronized boolean isDisqualified() {
        return disqualified;
    }

    public synchronized void setDisqualified(boolean disqualified) {
        this.disqualified = disqualified;
    }
    @Override
    public void run() {
        while (credits > 0) {
            try {
                Thread.sleep(new Random().nextInt(1000));
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt();
                break;
            }
        }
    }
}
