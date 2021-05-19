package diff;

public class Examples {
    public static void main(String[] args) {
        System.out.println("Hello world");

        String example = "Revert";
        String revertedExample = revert(example);
        String revertedRevertedExample = revert(revertedExample);
        System.out.println(revertedExample);
        System.out.println(revertedRevertedExample);

        String exampleSent = "This has to be returned";
        System.out.println(revertSentence(exampleSent));

        System.out.println("creating list");
        Node list = constructLinkedList();
        printList(list);
        Node reverted = revertList(list);
        System.out.println();
        printList(reverted);
        System.out.println();
        Node revertedRecursively = revertRecursively(reverted);
        printList(revertedRecursively);
        System.out.println();
        System.out.println(getHash());
    }

    private static String getHash() {
        Object a = null;

        return "" + a.hashCode();
    }

    // revert a string
    private static String revert(String str) {
        char[] arr = str.toCharArray();
        int begin = 0;
        int end = arr.length - 1;

        while (end > begin) {
            char temp = arr[begin];
            arr[begin] = arr[end];
            arr[end] = temp;

            begin++;
            end--;
        }

        return new String(arr);
    }

    private static String revertSentence(String sentence) {
        String s[] = sentence.split(" ");
        String res = "";
        for (int i = s.length - 1; i >= 0; i--) {
            res += s[i] + " ";
        }

        return res.trim();
    }

    private static class Node {
        private int data;
        private Node next;

        Node(int data) {
            this.data = data;
            next = null;
        }

        @Override
        public String toString() {
            return data + ", ";
        }
    }

    private static Node constructLinkedList() {
        Node head = null;
        Node tail = null;

        for (int i = 1; i <= 5; i++) {
            Node node = new Node(i);
            if (head == null) {
                head = node;
            } else {
                tail.next = node;
            }
            tail = node;
        }

        return head;
    }

    // iteratively
    private static Node revertList(Node head) {
        Node prev = null;
        Node current = head;
        while (current != null) {
            Node nextNode = current.next;
            current.next = prev;
            prev = current;
            current = nextNode;
        }

        return prev;
    }

    private static Node revertRecursively(Node node) {
        if (node == null) {
            return null;
        }
        if (node.next == null) {
            return node;
        }
        Node currentNode = revertRecursively(node.next);
        node.next.next = node;
        node.next = null;

        return currentNode;
    }

    private static void printList(Node node) {
        while (node != null) {
            System.out.print(node);
            node = node.next;
        }
    }
}
