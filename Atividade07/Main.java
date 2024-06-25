import java.io.*;
import java.util.*;

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
        for (String nome : nomesPaisesOrdenados) {
            System.out.println(nome);
        }
    }

    public static List<RegistroPais> carregaCSV(String filePath) {
        List<RegistroPais> registros = new ArrayList<>();
        try (BufferedReader br = new BufferedReader(new FileReader(filePath))) {
            String linha;
            while ((linha = br.readLine()) != null) {
                if (!linha.isEmpty()) {
                    registros.add(parseLinhaCSV(linha));
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return registros;
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
        int soma = 0;
        for (RegistroPais registro : registros) {
            if (registro.totalConfirmados >= limiar) {
                soma += registro.totalAtivos;
            }
        }
        return soma;
    }

    public static int somaCasosObitos(
        int quantidade,
        List<RegistroPais> registros
    ) {
        registros.sort(Comparator.comparingInt(r -> r.totalConfirmados));
        int soma = 0;
        for (int i = 0; i < Math.min(quantidade, registros.size()); i++) {
            soma += registros.get(i).totalObitos;
        }
        return soma;
    }

    public static List<RegistroPais> topNAtivos(
        int quantidade,
        List<RegistroPais> registros
    ) {
        registros.sort(
            (r1, r2) -> Integer.compare(r2.totalAtivos, r1.totalAtivos)
        );
        List<RegistroPais> topN = new ArrayList<>();
        for (int i = 0; i < Math.min(quantidade, registros.size()); i++) {
            topN.add(registros.get(i));
        }
        return topN;
    }

    public static List<String> paisesTopPorConfirmados(
        int quantidade,
        List<RegistroPais> registros
    ) {
        registros.sort(
            (r1, r2) ->
                Integer.compare(r2.totalConfirmados, r1.totalConfirmados)
        );
        List<String> nomes = new ArrayList<>();
        for (int i = 0; i < Math.min(quantidade, registros.size()); i++) {
            nomes.add(registros.get(i).nomePais);
        }
        Collections.sort(nomes);
        return nomes;
    }
}
