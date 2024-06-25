import java.io.*;
import java.util.*;
import java.util.stream.Collectors;

class RegistroPais {

    String nomePais;
    int totalConfirmados;
    int totalObitos;
    int totalRecuperados;
    int totalAtivos;

    public RegistroPais(
        String nomePais,
        int totalConfirmados,
        int totalObitos,
        int totalRecuperados,
        int totalAtivos
    ) {
        this.nomePais = nomePais;
        this.totalConfirmados = totalConfirmados;
        this.totalObitos = totalObitos;
        this.totalRecuperados = totalRecuperados;
        this.totalAtivos = totalAtivos;
    }

    @Override
    public String toString() {
        return (
            "RegistroPais{" +
            "nomePais='" +
            nomePais +
            '\'' +
            ", totalConfirmados=" +
            totalConfirmados +
            ", totalObitos=" +
            totalObitos +
            ", totalRecuperados=" +
            totalRecuperados +
            ", totalAtivos=" +
            totalAtivos +
            '}'
        );
    }
}

public class Main {

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        String[] input = scanner.nextLine().split(" ");
        int n1 = Integer.parseInt(input[0]);
        int n2 = Integer.parseInt(input[1]);
        int n3 = Integer.parseInt(input[2]);
        int n4 = Integer.parseInt(input[3]);

        List<RegistroPais> dadosPaises = carregaCSV("dados.csv");

        int somaCasosAtivosN1 = somaCasosAtivos(n1, dadosPaises);
        System.out.println(somaCasosAtivosN1);

        List<RegistroPais> registrosAtivosTopN2 = topNAtivos(n2, dadosPaises);
        int somaCasosObitosN3 = somaCasosObitos(n3, registrosAtivosTopN2);
        System.out.println(somaCasosObitosN3);

        List<String> nomesPaisesOrdenados = paisesTopPorConfirmados(
            n4,
            dadosPaises
        );
        nomesPaisesOrdenados.forEach(System.out::println);
    }

    public static List<RegistroPais> carregaCSV(String filePath) {
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            return br
                .lines()
                .filter(line -> !line.isEmpty())
                .map(Main::parseLinhaCSV)
                .collect(Collectors.toList());
        } catch (IOException e) {
            e.printStackTrace();
        }
        return Collections.emptyList();
    }

    public static RegistroPais parseLinhaCSV(String linha) {
        String[] campos = linha.split(",");
        String nomePais = campos[0];
        int totalConfirmados = Integer.parseInt(campos[1]);
        int totalObitos = Integer.parseInt(campos[2]);
        int totalRecuperados = Integer.parseInt(campos[3]);
        int totalAtivos = Integer.parseInt(campos[4]);

        return new RegistroPais(
            nomePais,
            totalConfirmados,
            totalObitos,
            totalRecuperados,
            totalAtivos
        );
    }

    public static int somaCasosAtivos(
        int limiar,
        List<RegistroPais> registros
    ) {
        return registros
            .stream()
            .filter(registro -> registro.totalConfirmados >= limiar)
            .mapToInt(registro -> registro.totalAtivos)
            .sum();
    }

    public static int somaCasosObitos(
        int quantidade,
        List<RegistroPais> registros
    ) {
        return registros
            .stream()
            .sorted(
                Comparator.comparingInt((RegistroPais r) -> r.totalConfirmados)
            )
            .limit(quantidade)
            .mapToInt(registro -> registro.totalObitos)
            .sum();
    }

    public static List<RegistroPais> topNAtivos(
        int quantidade,
        List<RegistroPais> registros
    ) {
        return registros
            .stream()
            .sorted(
                Comparator.comparingInt(
                    (RegistroPais r) -> r.totalAtivos
                ).reversed()
            )
            .limit(quantidade)
            .collect(Collectors.toList());
    }

    public static List<String> paisesTopPorConfirmados(
        int quantidade,
        List<RegistroPais> registros
    ) {
        return registros
            .stream()
            .sorted(
                Comparator.comparingInt(
                    (RegistroPais r) -> r.totalConfirmados
                ).reversed()
            )
            .limit(quantidade)
            .map(registro -> registro.nomePais)
            .sorted()
            .collect(Collectors.toList());
    }
}
