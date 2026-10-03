# -*- coding: utf-8 -*-
"""
Created on Sat Oct  3 18:14:16 2026

@author: lourd
"""

# -*- coding: utf-8 -*-

import streamlit as st
import pandas as pd
import altair as alt
import datetime


# ============================================================
# CONFIGURACIÓN GENERAL
# ============================================================

ARCHIVO_EXCEL = "padel_2026.xlsx"

st.set_page_config(
    page_title="Campeonato de Pádel 2026",
    page_icon="🏆",
    layout="wide"
)

st.title("🏆 Campeonato de Pádel - SGFAL 2026")


# ============================================================
# BARRA LATERAL
# ============================================================

pagina = st.sidebar.radio(
    "Navegación",
    [
        "Clasificación 🏅",
        "Participantes 👥",
        "Informe semanal 🗞️",
        "Estadísticas 📊",
        "Campeonato Final 🏆"
    ]
)


# ============================================================
# 1. CLASIFICACIÓN
# ============================================================

if pagina == "Clasificación 🏅":

    st.header("📈 Clasificación por grupo y vuelta")

    # --------------------------------------------------------
    # Cargar datos
    # --------------------------------------------------------

    try:
        clasif = pd.read_excel(
            ARCHIVO_EXCEL,
            sheet_name="clasificacion"
        )

        resultados = pd.read_excel(
            ARCHIVO_EXCEL,
            sheet_name="resultados"
        )

    except FileNotFoundError:

        st.error(
            f"❌ No se encontró el archivo '{ARCHIVO_EXCEL}'."
        )

        st.stop()

    except ValueError as e:

        st.error(
            f"❌ Hay un problema con las hojas del Excel: {e}"
        )

        st.stop()


    # Normalizar nombres de columnas
    clasif.columns = clasif.columns.str.strip().str.upper()
    resultados.columns = resultados.columns.str.strip().str.upper()


    # --------------------------------------------------------
    # Grupos disponibles
    # --------------------------------------------------------

    grupos_disponibles = (
        clasif["GRUPO"]
        .dropna()
        .astype(str)
        .unique()
        .tolist()
    )

    # --------------------------------------------------------
    # Vueltas disponibles
    # --------------------------------------------------------

    vueltas_disponibles = (
        resultados["VUELTA"]
        .dropna()
        .astype(str)
        .unique()
        .tolist()
    )

    if len(vueltas_disponibles) == 0:
        vueltas_disponibles = ["1ª vuelta", "2ª vuelta"]


    # --------------------------------------------------------
    # Selectores
    # --------------------------------------------------------

    col1, col2 = st.columns(2)

    grupo = col1.selectbox(
        "Selecciona el grupo:",
        grupos_disponibles
    )

    vuelta = col2.selectbox(
        "Selecciona la vuelta:",
        vueltas_disponibles
    )


    # --------------------------------------------------------
    # Filtrar clasificación
    # --------------------------------------------------------

    clasif_f = clasif[
        clasif["GRUPO"].astype(str).str.lower()
        == grupo.lower()
    ].copy()


    # --------------------------------------------------------
    # Filtrar resultados
    # --------------------------------------------------------

    resultados_f = resultados[
        (
            resultados["GRUPO"]
            .astype(str)
            .str.lower()
            == grupo.lower()
        )
        &
        (
            resultados["VUELTA"]
            .astype(str)
            .str.lower()
            == vuelta.lower()
        )
    ].copy()


    # --------------------------------------------------------
    # Progreso de partidos
    # --------------------------------------------------------

    parejas_grupo = clasif_f["PAREJA"].nunique()

    partidos_totales = int(
        parejas_grupo * (parejas_grupo - 1) / 2
    )

    partidos_jugados = (
        resultados_f["RESULTADO_P1P2"]
        .notna()
        .sum()
    )

    if partidos_totales > 0:

        porcentaje = (
            partidos_jugados /
            partidos_totales
        ) * 100

    else:

        porcentaje = 0


    st.markdown(
        f"### 🏁 Progreso de partidos jugados "
        f"({vuelta} - {grupo})"
    )

    st.progress(
        min(porcentaje / 100, 1.0)
    )

    st.write(
        f"**Partidos jugados:** "
        f"{partidos_jugados} / "
        f"{partidos_totales} "
        f"→ ({porcentaje:.1f}%) completado"
    )


    # --------------------------------------------------------
    # Tabla clasificación
    # --------------------------------------------------------

    st.subheader(
        f"📊 Clasificación - {grupo}"
    )

    columnas_clasificacion = [

        "CLASIFICACION",
        "PAREJA",
        "PUNTOS",
        "P. JUGADOS",
        "P. GANADOS",
        "P. EMPATADOS",
        "P. PERDIDOS",
        "SET GANADOS",
        "SET PERDIDOS"

    ]

    columnas_existentes = [
        c for c in columnas_clasificacion
        if c in clasif_f.columns
    ]

    st.dataframe(
        clasif_f[columnas_existentes],
        use_container_width=True,
        hide_index=True
    )


    # --------------------------------------------------------
    # Matriz de resultados
    # --------------------------------------------------------

    parejas = clasif_f["PAREJA"].tolist()

    matriz = pd.DataFrame(
        index=parejas,
        columns=parejas
    )


    for _, row in resultados_f.iterrows():

        p1 = row["PAREJA1"]
        p2 = row["PAREJA2"]

        r12 = row.get(
            "RESULTADO_P1P2",
            ""
        )

        r21 = row.get(
            "RESULTADO_P2P1",
            ""
        )

        if p1 in matriz.index and p2 in matriz.columns:
            matriz.loc[p1, p2] = r12

        if p2 in matriz.index and p1 in matriz.columns:
            matriz.loc[p2, p1] = r21


    for pareja in parejas:
        matriz.loc[pareja, pareja] = "🎾"


    st.subheader(
        f"🎾 Resultados {vuelta}"
    )

    st.dataframe(
        matriz,
        use_container_width=True
    )


# ============================================================
# 2. PARTICIPANTES
# ============================================================

elif pagina == "Participantes 👥":

    st.header(
        "👥 Información de los participantes"
    )


    try:

        participantes = pd.read_excel(
            ARCHIVO_EXCEL,
            sheet_name="participantes"
        )

    except FileNotFoundError:

        st.error(
            f"❌ No se encontró "
            f"'{ARCHIVO_EXCEL}'."
        )

        st.stop()

    except ValueError:

        st.error(
            "❌ No se encontró la hoja "
            "'participantes'."
        )

        st.stop()


    participantes.columns = (
        participantes.columns
        .str.strip()
        .str.upper()
    )


    # --------------------------------------------------------
    # Obtener grupos automáticamente
    # --------------------------------------------------------

    grupos = (
        participantes["GRUPO"]
        .dropna()
        .astype(str)
        .unique()
        .tolist()
    )


    grupo = st.selectbox(
        "Selecciona el grupo:",
        ["Todos"] + grupos
    )


    if grupo != "Todos":

        df = participantes[
            participantes["GRUPO"]
            .astype(str)
            .str.lower()
            == grupo.lower()
        ].copy()

    else:

        df = participantes.copy()


    # --------------------------------------------------------
    # Mostrar parejas
    # --------------------------------------------------------

    for grupo_nombre in grupos:

        df_grupo = df[
            df["GRUPO"]
            .astype(str)
            .str.lower()
            == grupo_nombre.lower()
        ]


        if df_grupo.empty:
            continue


        st.markdown(
            f"## 🎾 {grupo_nombre}"
        )


        parejas_grupo = (
            df_grupo.groupby("PAREJA")
        )


        for pareja_id, data in parejas_grupo:

            st.markdown(
                f"### Pareja {pareja_id}"
            )


            columnas = st.columns(2)


            for i, (_, row) in enumerate(
                data.iterrows()
            ):

                with columnas[i % 2]:

                    st.markdown(
                        f"""
                        ### {row['NOMBRE']}
                        ✉️ {row['CORREO ELECTRONICO']}
                        """
                    )


            st.divider()


# ============================================================
# 3. INFORME SEMANAL
# ============================================================

elif pagina == "Informe semanal 🗞️":

    st.header(
        "🗞️ Informe semanal del campeonato"
    )


    # Aquí iremos añadiendo los informes de 2026
    #
    # Ejemplo:
    #
    # "05/10/2026": """
    # 🗓️ **Informe del 05/10/2026**
    #
    # Güenas a tod@s...
    # """


    informes = {

    }


    if len(informes) == 0:

        st.info(
            "📅 Todavía no hay informes "
            "semanales publicados para 2026."
        )

    else:

        fechas_ordenadas = sorted(

            informes.keys(),

            key=lambda f:
            datetime.datetime.strptime(
                f,
                "%d/%m/%Y"
            ),

            reverse=True

        )


        fecha_sel = st.selectbox(
            "📅 Selecciona el día del informe:",
            fechas_ordenadas
        )


        st.markdown(
            informes[fecha_sel]
        )


# ============================================================
# 4. ESTADÍSTICAS
# ============================================================

elif pagina == "Estadísticas 📊":

    st.header(
        "📊 Estadísticas de las parejas"
    )


    try:

        hist = pd.read_excel(
            ARCHIVO_EXCEL,
            sheet_name="historial_partidos"
        )

    except FileNotFoundError:

        st.error(
            f"❌ No se encontró "
            f"'{ARCHIVO_EXCEL}'."
        )

        st.stop()

    except ValueError:

        st.warning(
            "⚠️ Todavía no existe la hoja "
            "'historial_partidos'."
        )

        st.stop()


    hist.columns = (
        hist.columns
        .str.strip()
        .str.upper()
    )


    if hist.empty:

        st.info(
            "📊 Todavía no hay partidos "
            "suficientes para mostrar estadísticas."
        )

        st.stop()


    hist["GRUPO"] = (
        hist["GRUPO"]
        .astype(str)
        .str.title()
    )


    grupos = (
        hist["GRUPO"]
        .dropna()
        .unique()
        .tolist()
    )


    grupo_sel = st.selectbox(
        "Selecciona el grupo:",
        grupos
    )


    parejas = (
        hist[
            hist["GRUPO"] == grupo_sel
        ]["PAREJA"]
        .dropna()
        .unique()
        .tolist()
    )


    pareja_sel = st.selectbox(
        "Selecciona una pareja (o 'Todas'):",
        ["Todas"] + parejas
    )


    # --------------------------------------------------------
    # Filtrar
    # --------------------------------------------------------

    if pareja_sel != "Todas":

        df_plot = hist[
            (hist["GRUPO"] == grupo_sel)
            &
            (hist["PAREJA"] == pareja_sel)
        ]

    else:

        df_plot = hist[
            hist["GRUPO"] == grupo_sel
        ]


    # --------------------------------------------------------
    # Gráfico evolución
    # --------------------------------------------------------

    st.subheader(
        "📈 Evolución de puntos acumulados"
    )


    chart = (

        alt.Chart(df_plot)

        .mark_line(
            point=True,
            strokeWidth=3
        )

        .encode(

            x=alt.X(
                "PARTIDO:Q",
                title="Número de partido"
            ),

            y=alt.Y(
                "PUNTOS_ACUM:Q",
                title="Puntos acumulados"
            ),

            color=alt.Color(
                "PAREJA:N",
                legend=alt.Legend(
                    title="Pareja"
                ),
                scale=alt.Scale(
                    scheme="set2"
                )
            ),

            tooltip=[
                "PAREJA",
                "RESULTADO",
                "PUNTOS_ACUM",
                "PG",
                "PE",
                "PP"
            ]

        )

        .properties(
            height=420,
            width="container"
        )

    )


    st.altair_chart(
        chart,
        use_container_width=True
    )


    # --------------------------------------------------------
    # Resumen
    # --------------------------------------------------------

    st.subheader(
        "📋 Rendimiento acumulado"
    )


    resumen = (

        df_plot

        .groupby("PAREJA")

        .agg({

            "PG": "max",
            "PE": "max",
            "PP": "max",
            "PUNTOS_ACUM": "max"

        })

        .reset_index()

        .rename(
            columns={

                "PG": "Ganados",
                "PE": "Empatados",
                "PP": "Perdidos",
                "PUNTOS_ACUM": "Puntos Totales"

            }
        )

    )


    st.dataframe(
        resumen,
        use_container_width=True,
        hide_index=True
    )


# ============================================================
# 5. CAMPEONATO FINAL
# ============================================================

elif pagina == "Campeonato Final 🏆":

    st.header(
        "🏆 Cuadro final - Campeonato 2026"
    )

    st.info(
        "Aquí se visualizará el cuadro "
        "de semifinales y finales 🏁."
    )