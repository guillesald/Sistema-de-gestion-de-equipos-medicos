-- Sistema de Gestión de Equipos Médicos, DDL básico (excluyendo tablas de Directus)
-- =========================================
-- PostgreSQL 16
-- =========================================
-- Recomendado: ejecutar en una BD vacía
-- =========================================
-- 0) Extensiones / configuración opcional
-- =========================================
-- CREATE EXTENSION IF NOT EXISTS plpgsql;

-- =========================================
-- 1) Catálogos (lados "1")
-- =========================================
CREATE TABLE IF NOT EXISTS "RIESGO" (
  "Id"   SMALLINT PRIMARY KEY,
  "Tipo" TEXT NOT NULL UNIQUE
);

CREATE TABLE IF NOT EXISTS "UBICACION" (
  "Id" SMALLSERIAL PRIMARY KEY,
  "Lugar" TEXT
);

CREATE TABLE IF NOT EXISTS "SERVICIO" (
  "Id" SMALLSERIAL PRIMARY KEY,
  "Nombre" TEXT NOT NULL,
  "Descripcion" TEXT
);

CREATE TABLE IF NOT EXISTS "PROVEEDOR_COMPRA" (
  "Id" SMALLSERIAL PRIMARY KEY,
  "Nombre" TEXT NOT NULL
);

CREATE TABLE IF NOT EXISTS "PROVEEDOR_MANTENIMIENTO" (
  "Id" SMALLSERIAL PRIMARY KEY,
  "Nombre" TEXT NOT NULL
);

-- =========================================
-- 2) Núcleo
-- =========================================
CREATE TABLE IF NOT EXISTS "EQUIPO" (
  "CodigoEquipo"   TEXT PRIMARY KEY,
  "Descripcion"    TEXT,
  "NumeroDeSerie"  TEXT,
  "Estado"         TEXT NOT NULL DEFAULT 'OPERATIVO',
  "FechaAlta"      DATE NOT NULL,
  "FechaBaja"      DATE,
  "Marca"          TEXT,
  "Modelo"         TEXT,
  "VidaUtil"       TEXT,
  "RiesgoId"       SMALLINT NOT NULL REFERENCES "RIESGO"("Id"),
  "UbicacionId"    SMALLINT     REFERENCES "UBICACION"("Id"), -- NULL permitido
  "ServicioId"     SMALLINT NOT NULL REFERENCES "SERVICIO"("Id"),
  CONSTRAINT "chk_FechaBaja_vs_FechaAlta"
    CHECK ("FechaBaja" IS NULL OR "FechaBaja" >= "FechaAlta"),
  CONSTRAINT "chk_equipo_estado_upper"
    CHECK (UPPER("Estado") IN ('BAJA','DEPOSITO','OPERATIVO'))
);

CREATE INDEX IF NOT EXISTS idx_equipo_estado        ON "EQUIPO" ("Estado");
CREATE INDEX IF NOT EXISTS idx_equipo_numero_serie  ON "EQUIPO" ("NumeroDeSerie");
CREATE INDEX IF NOT EXISTS idx_equipo_fecha_alta    ON "EQUIPO" ("FechaAlta");
CREATE INDEX IF NOT EXISTS idx_equipo_ubicacion     ON "EQUIPO" ("UbicacionId");
CREATE INDEX IF NOT EXISTS idx_equipo_riesgo        ON "EQUIPO" ("RiesgoId");
CREATE INDEX IF NOT EXISTS idx_equipo_servicio      ON "EQUIPO" ("ServicioId");

-- =========================================
-- 3) Descripción contable + puente proveedores mantenimiento
-- =========================================
CREATE TABLE IF NOT EXISTS "DESCRIPCION_CONTABLE" (
  "Id" BIGSERIAL PRIMARY KEY,
  "CodigoEquipo"      TEXT NOT NULL REFERENCES "EQUIPO"("CodigoEquipo") ON DELETE CASCADE,
  "ResponsableCompra" TEXT,
  "NumeroFactura"     TEXT,
  "NumeroRemito"      TEXT,
  "Valor"             NUMERIC(14,2),
  "ProveedorCompraId" SMALLINT NOT NULL REFERENCES "PROVEEDOR_COMPRA"("Id")
);

CREATE INDEX IF NOT EXISTS idx_dc_equipo           ON "DESCRIPCION_CONTABLE"("CodigoEquipo");
CREATE INDEX IF NOT EXISTS idx_dc_prov_compra      ON "DESCRIPCION_CONTABLE"("ProveedorCompraId");

CREATE TABLE IF NOT EXISTS "DESCRIPCION_CONTABLE_PROV_MANT" (
  "DescripcionContableId" BIGINT   NOT NULL REFERENCES "DESCRIPCION_CONTABLE"("Id") ON DELETE CASCADE,
  "ProveedorMantId"       SMALLINT NOT NULL REFERENCES "PROVEEDOR_MANTENIMIENTO"("Id"),
  PRIMARY KEY ("DescripcionContableId","ProveedorMantId")
);

-- =========================================
-- 4) Correctivo: Falla + Intervención + Responsables
-- =========================================
CREATE TABLE IF NOT EXISTS "FALLA" (
  "Id"           BIGSERIAL PRIMARY KEY,
  "CodigoEquipo" TEXT NOT NULL REFERENCES "EQUIPO"("CodigoEquipo") ON DELETE CASCADE,
  "Fecha"        DATE NOT NULL,
  "Descripcion"  TEXT
);
CREATE INDEX IF NOT EXISTS idx_falla_equipo_fecha ON "FALLA"("CodigoEquipo","Fecha");

CREATE TABLE IF NOT EXISTS "INTERVENCION_CORRECTIVA" (
  "Id"          BIGSERIAL PRIMARY KEY,
  "FallaId"     BIGINT NOT NULL REFERENCES "FALLA"("Id") ON DELETE CASCADE,
  "Tarea"       TEXT NOT NULL,
  "FechaReporte" DATE,
  "Fecha"        DATE NOT NULL
  -- Nota: si decidís persistir horas desde reporte, agregar:
  -- ALTER TABLE "INTERVENCION_CORRECTIVA" ADD COLUMN "HorasDesdeReporte" INTEGER DEFAULT 0;
);
CREATE INDEX IF NOT EXISTS idx_ic_falla ON "INTERVENCION_CORRECTIVA"("FallaId");
CREATE INDEX IF NOT EXISTS idx_ic_fecha ON "INTERVENCION_CORRECTIVA"("Fecha");

CREATE TABLE IF NOT EXISTS "RESPONSABLE" (
  "Id" SMALLSERIAL PRIMARY KEY,
  "Nombre" TEXT NOT NULL,
  "Area"   TEXT,
  "Tipo"   TEXT,
  CONSTRAINT "RESPONSABLE_Tipo_check" CHECK ("Tipo" IN ('INGENIERIA CLINICA','TERCERIZADO'))
);

CREATE TABLE IF NOT EXISTS "RESPONSABLE_INTERVENCION_CORRECTIVA" (
  "IntervencionId" BIGINT   NOT NULL REFERENCES "INTERVENCION_CORRECTIVA"("Id") ON DELETE CASCADE,
  "ResponsableId"  SMALLINT NOT NULL REFERENCES "RESPONSABLE"("Id"),
  PRIMARY KEY ("IntervencionId","ResponsableId")
);

-- =========================================
-- 5) Preventivo: Cronograma + Intervenciones (acumulativo)
-- =========================================
CREATE TABLE IF NOT EXISTS "CRONOGRAMA_PREVENTIVO" (
  "Id"                   BIGSERIAL PRIMARY KEY,
  "CodigoEquipo"         TEXT NOT NULL REFERENCES "EQUIPO"("CodigoEquipo") ON DELETE CASCADE,
  "DescripcionTarea"     TEXT NOT NULL,
  "FrecuenciaDias"       INTEGER NOT NULL CHECK ("FrecuenciaDias" > 0),
  "UltimaIntervencion"   DATE,
  "ProximaIntervencion"  DATE,
  "Estado"               TEXT NOT NULL DEFAULT 'CUMPLIDO'
                           CHECK ("Estado" IN ('CUMPLIDO','PROXIMO','VENCIDO')),
  CONSTRAINT "uq_crono_equipo_tarea" UNIQUE ("CodigoEquipo","DescripcionTarea")
);
CREATE INDEX IF NOT EXISTS idx_crono_equipo ON "CRONOGRAMA_PREVENTIVO"("CodigoEquipo");
CREATE INDEX IF NOT EXISTS idx_crono_estado ON "CRONOGRAMA_PREVENTIVO"("Estado");

-- Índice parcial para evitar duplicados de "parche" por equipo,
-- cuando la tarea es "NO IDENTIFICADA (REGISTRO)"
CREATE UNIQUE INDEX IF NOT EXISTS idx_crono_parche_unico
  ON "CRONOGRAMA_PREVENTIVO"("CodigoEquipo")
  WHERE "DescripcionTarea" ILIKE '%NO IDENTIFICADA (REGISTRO)%';

CREATE TABLE IF NOT EXISTS "INTERVENCION_PREVENTIVA" (
  "Id"            BIGSERIAL PRIMARY KEY,
  "CronogramaId"  BIGINT NOT NULL REFERENCES "CRONOGRAMA_PREVENTIVO"("Id") ON DELETE CASCADE,
  "Fecha"         DATE   NOT NULL,
  "Observaciones" TEXT,
  CONSTRAINT "uq_crono_fecha" UNIQUE ("CronogramaId","Fecha")
);
CREATE INDEX IF NOT EXISTS idx_ip_crono_fecha ON "INTERVENCION_PREVENTIVA"("CronogramaId","Fecha");

CREATE TABLE IF NOT EXISTS "RESPONSABLE_INTERVENCION_PREVENTIVA" (
  "IntervencionId" BIGINT   NOT NULL REFERENCES "INTERVENCION_PREVENTIVA"("Id") ON DELETE CASCADE,
  "ResponsableId"  SMALLINT NOT NULL REFERENCES "RESPONSABLE"("Id"),
  PRIMARY KEY ("IntervencionId","ResponsableId")
);


-- =========================================
-- 6) Vistas
-- =========================================
CREATE OR REPLACE VIEW public.vw_cronograma_ordenado AS
SELECT
  c."CodigoEquipo",
  e."Descripcion"     AS "Equipo",
  s."Descripcion"     AS "Servicio",
  c."DescripcionTarea",
  c."FrecuenciaDias",
  c."UltimaIntervencion",
  c."ProximaIntervencion",
  c."Estado",
  CASE
    WHEN c."Estado"='VENCIDO' THEN 1
    WHEN c."Estado"='PROXIMO' THEN 2
    WHEN c."Estado"='CUMPLIDO' THEN 3
    ELSE 4
  END AS "OrdenEstado"
FROM "CRONOGRAMA_PREVENTIVO" c
JOIN "EQUIPO" e ON e."CodigoEquipo" = c."CodigoEquipo"
LEFT JOIN "SERVICIO" s ON s."Id" = e."ServicioId"
ORDER BY
  CASE
    WHEN c."Estado"='VENCIDO' THEN 1
    WHEN c."Estado"='PROXIMO' THEN 2
    WHEN c."Estado"='CUMPLIDO' THEN 3
    ELSE 4
  END,
  c."ProximaIntervencion";

CREATE OR REPLACE VIEW public.vw_intervenciones AS
-- ===== PREVENTIVAS =====
SELECT
  'PREVENTIVA'::text                    AS tipo,
  ip."Id"                               AS intervencion_id,
  NULL::bigint                          AS falla_id,
  cp."Id"                               AS cronograma_id,
  cp."CodigoEquipo"                     AS codigo_equipo,
  e."Descripcion"                       AS descripcion_equipo,
  s."Nombre"                            AS servicio,
  u."Lugar"                             AS ubicacion,
  r."Tipo"                              AS riesgo,
  cp."DescripcionTarea"                 AS tarea,
  ip."Fecha"                            AS fecha_intervencion,
  NULL::date                            AS fecha_reporte,
  NULL::date                            AS fecha_falla,
  ip."Observaciones"                    AS observaciones,
  cp."Estado"                           AS estado,
  COALESCE(STRING_AGG(DISTINCT resp."Nombre", ' | ')
           FILTER (WHERE resp."Nombre" IS NOT NULL), '')       AS responsables
FROM "INTERVENCION_PREVENTIVA" ip
JOIN "CRONOGRAMA_PREVENTIVO" cp ON cp."Id" = ip."CronogramaId"
JOIN "EQUIPO" e                  ON e."CodigoEquipo" = cp."CodigoEquipo"
LEFT JOIN "SERVICIO" s           ON s."Id" = e."ServicioId"
LEFT JOIN "UBICACION" u          ON u."Id" = e."UbicacionId"
LEFT JOIN "RIESGO" r             ON r."Id" = e."RiesgoId"
LEFT JOIN "RESPONSABLE_INTERVENCION_PREVENTIVA" rip ON rip."IntervencionId" = ip."Id"
LEFT JOIN "RESPONSABLE" resp     ON resp."Id" = rip."ResponsableId"
GROUP BY ip."Id", cp."Id", cp."CodigoEquipo", e."Descripcion",
         s."Nombre", u."Lugar", r."Tipo", cp."DescripcionTarea",
         cp."Estado", ip."Fecha", ip."Observaciones"

UNION ALL

-- ===== CORRECTIVAS =====
SELECT
  'CORRECTIVA'::text                   AS tipo,
  ic."Id"                              AS intervencion_id,
  f."Id"                               AS falla_id,
  NULL::bigint                         AS cronograma_id,
  f."CodigoEquipo"                     AS codigo_equipo,
  e."Descripcion"                      AS descripcion_equipo,
  s."Nombre"                           AS servicio,
  u."Lugar"                            AS ubicacion,
  r."Tipo"                             AS riesgo,
  ic."Tarea"                           AS tarea,
  ic."Fecha"                           AS fecha_intervencion,
  ic."FechaReporte"                    AS fecha_reporte,
  f."Fecha"                            AS fecha_falla,
  NULL::text                           AS observaciones,
  NULL::text                           AS estado,
  COALESCE(STRING_AGG(DISTINCT resp2."Nombre", ' | ')
           FILTER (WHERE resp2."Nombre" IS NOT NULL), '')      AS responsables
FROM "INTERVENCION_CORRECTIVA" ic
JOIN "FALLA" f         ON f."Id" = ic."FallaId"
JOIN "EQUIPO" e        ON e."CodigoEquipo" = f."CodigoEquipo"
LEFT JOIN "SERVICIO" s ON s."Id" = e."ServicioId"
LEFT JOIN "UBICACION" u ON u."Id" = e."UbicacionId"
LEFT JOIN "RIESGO" r   ON r."Id" = e."RiesgoId"
LEFT JOIN "RESPONSABLE_INTERVENCION_CORRECTIVA" ric ON ric."IntervencionId" = ic."Id"
LEFT JOIN "RESPONSABLE" resp2 ON resp2."Id" = ric."ResponsableId"
GROUP BY ic."Id", f."Id", f."CodigoEquipo", e."Descripcion",
         s."Nombre", u."Lugar", r."Tipo", ic."Tarea",
         ic."Fecha", ic."FechaReporte", f."Fecha";

-- O2 y CO2
CREATE TABLE public."O2_Y_CO2" (
    id integer NOT NULL,
    "Fecha" date,
    "Carga" integer NOT NULL,
    "Tipo" varchar(255),
    "Unidad" varchar(255)
);






/* ============================================================
   FUNCIONES PARA INTERVENCIÓN CORRECTIVA → ACTUALIZAR FALLA
   ============================================================ */

CREATE FUNCTION public.fn_ic_before_insert_upd_falla() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
  v_prev_estado text;
  v_prev_fecha_cierre timestamp;
  v_new_estado text;
BEGIN
  SELECT "Estado","FechaCierre"
    INTO v_prev_estado, v_prev_fecha_cierre
  FROM "FALLA"
  WHERE "Id" = NEW."FallaId";

  NEW."PrevEstadoFalla" := v_prev_estado;
  NEW."PrevFechaCierre" := v_prev_fecha_cierre;

  IF NEW."Resuelto" IS TRUE THEN
    v_new_estado := 'RESUELTO';
  ELSE
    IF v_prev_estado IS NULL OR v_prev_estado = 'PENDIENTE' THEN
      v_new_estado := 'EN PROCESO';
    ELSE
      v_new_estado := v_prev_estado;
    END IF;
  END IF;

  IF v_new_estado = 'RESUELTO' THEN
    UPDATE "FALLA"
      SET "Estado" = v_new_estado,
          "FechaCierre" = NEW."Fecha"
    WHERE "Id" = NEW."FallaId";
  ELSE
    UPDATE "FALLA"
      SET "Estado" = v_new_estado
    WHERE "Id" = NEW."FallaId";
  END IF;

  RETURN NEW;
END$$;

--------------------------------------------------------------

CREATE FUNCTION public.fn_ic_after_delete_revert_falla() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF OLD."PrevEstadoFalla" IS NULL AND OLD."PrevFechaCierre" IS NULL THEN
    UPDATE "FALLA"
      SET "Estado" = NULL,
          "FechaCierre" = NULL
    WHERE "Id" = OLD."FallaId";
  ELSE
    UPDATE "FALLA"
      SET "Estado" = OLD."PrevEstadoFalla",
          "FechaCierre" = OLD."PrevFechaCierre"
    WHERE "Id" = OLD."FallaId";
  END IF;

  RETURN OLD;
END$$;

--------------------------------------------------------------

CREATE FUNCTION public.fn_ic_post_insert_update() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
    v_prev_estado text;
    v_prev_fecha_cierre timestamp;
    v_new_estado text;
BEGIN
    SELECT COALESCE("Estado",'PENDIENTE'), "FechaCierre"
      INTO v_prev_estado, v_prev_fecha_cierre
    FROM "FALLA"
    WHERE "Id" = NEW."FallaId";

    NEW."PrevEstadoFalla" := v_prev_estado;
    NEW."PrevFechaCierre" := v_prev_fecha_cierre;

    IF NEW."Resuelto" IS TRUE THEN
        v_new_estado := 'RESUELTO';
    ELSE
        IF v_prev_estado = 'PENDIENTE' THEN
            v_new_estado := 'EN PROCESO';
        ELSE
            v_new_estado := v_prev_estado;
        END IF;
    END IF;

    UPDATE "FALLA"
       SET "Estado" = v_new_estado,
           "FechaCierre" = CASE WHEN NEW."Resuelto" IS TRUE THEN NOW() ELSE "FechaCierre" END
     WHERE "Id" = NEW."FallaId";

    RETURN NEW;
END;
$$;

--------------------------------------------------------------

CREATE FUNCTION public.fn_revert_falla_desde_ic() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
    v_pendiente_id INT;
BEGIN
    SELECT "Id" INTO v_pendiente_id
    FROM "ESTADO_FALLA"
    WHERE "Nombre" = 'PENDIENTE';

    UPDATE "FALLA"
       SET "EstadoId" = v_pendiente_id,
           "FechaCierre" = NULL
     WHERE "Id" = OLD."FallaId";

    RETURN OLD;
END;
$$;

--------------------------------------------------------------

CREATE FUNCTION public.fn_upd_estado_falla_desde_ic() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
    v_prev_estado text;
    v_new_estado text;
BEGIN
    SELECT "Estado"
      INTO v_prev_estado
    FROM "FALLA"
    WHERE "Id" = NEW."FallaId";

    IF NEW."Resuelto" IS TRUE THEN
        v_new_estado := 'RESUELTO';
    ELSE
        IF v_prev_estado IS NULL OR v_prev_estado = 'PENDIENTE' THEN
            v_new_estado := 'EN PROCESO';
        ELSE
            v_new_estado := v_prev_estado;
        END IF;

        IF v_new_estado NOT IN ('PENDIENTE','EN PROCESO','CERRADO','RESUELTO') THEN
            v_new_estado := 'EN PROCESO';
        END IF;
    END IF;

    UPDATE "FALLA"
       SET "Estado" = v_new_estado,
           "FechaCierre" = CASE WHEN NEW."Resuelto" IS TRUE THEN NOW() ELSE "FechaCierre" END
     WHERE "Id" = NEW."FallaId";

    RETURN NEW;
END;
$$;

--------------------------------------------------------------

CREATE FUNCTION public.fn_upd_falla_desde_ic() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
    v_prev_estado_id INT;
    v_new_estado_id  INT;
BEGIN
    SELECT "EstadoId"
      INTO v_prev_estado_id
    FROM "FALLA"
    WHERE "Id" = NEW."FallaId";

    NEW."PrevEstadoFalla" := v_prev_estado_id;
    NEW."PrevFechaCierre" := (SELECT "FechaCierre" FROM "FALLA" WHERE "Id" = NEW."FallaId");

    IF NEW."Resuelto" IS TRUE THEN
        SELECT "Id" INTO v_new_estado_id FROM "ESTADO_FALLA" WHERE "Nombre" = 'RESUELTO';
    ELSE
        IF v_prev_estado_id IS NULL THEN
            SELECT "Id" INTO v_new_estado_id FROM "ESTADO_FALLA" WHERE "Nombre" = 'EN_PROCESO';
        ELSE
            v_new_estado_id := v_prev_estado_id;
        END IF;
    END IF;

    UPDATE "FALLA"
       SET "EstadoId" = v_new_estado_id,
           "FechaCierre" = CASE WHEN NEW."Resuelto" IS TRUE THEN NOW() ELSE "FechaCierre" END
     WHERE "Id" = NEW."FallaId";

    RETURN NEW;
END;
$$;

/* ============================================================
   FUNCIONES PARA CRONOGRAMA PREVENTIVO
   ============================================================ */

CREATE FUNCTION public.fn_upd_cronograma_estado() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
    v_proxima date;
    v_diff integer;
    v_estado text;
BEGIN
    IF NEW."UltimaIntervencion" IS NULL THEN
        v_proxima := CURRENT_DATE + COALESCE(NEW."FrecuenciaDias", 365);
    ELSE
        v_proxima := NEW."UltimaIntervencion" + COALESCE(NEW."FrecuenciaDias", 365);
    END IF;

    v_diff := v_proxima - CURRENT_DATE;

    IF v_diff < 0 THEN
        v_estado := 'VENCIDO';
    ELSIF v_diff < 7 THEN
        v_estado := 'PROXIMO';
    ELSE
        v_estado := 'CUMPLIDO';
    END IF;

    NEW."ProximaIntervencion" := v_proxima;
    NEW."Estado" := v_estado;

    RETURN NEW;
END$$;

--------------------------------------------------------------

CREATE FUNCTION public.fn_upd_cronograma_desde_cp() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
    v_proxima DATE;
    v_diff INTEGER;
BEGIN
    v_proxima := (NEW."UltimaIntervencion" + (NEW."FrecuenciaDias" || ' days')::interval)::date;

    v_diff := v_proxima - CURRENT_DATE;

    IF v_diff < 0 THEN
        NEW."Estado" := 'VENCIDO';
    ELSIF v_diff < 7 THEN
        NEW."Estado" := 'PROXIMO';
    ELSE
        NEW."Estado" := 'CUMPLIDO';
    END IF;

    NEW."ProximaIntervencion" := v_proxima;

    RETURN NEW;
END;
$$;

--------------------------------------------------------------

CREATE FUNCTION public.fn_upd_cronograma_desde_ip() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
  v_frec_dias  INTEGER;
  v_ultima     DATE;
  v_proxima    DATE;
  v_estado     TEXT;
  v_diff       INTEGER;
BEGIN
  SELECT "FrecuenciaDias","UltimaIntervencion"
    INTO v_frec_dias, v_ultima
  FROM "CRONOGRAMA_PREVENTIVO"
  WHERE "Id" = NEW."CronogramaId";

  NEW."UltimaAntesInsert" := v_ultima;

  IF v_ultima IS NULL OR NEW."Fecha" > v_ultima THEN
    v_ultima := NEW."Fecha";
  END IF;

  v_proxima := (v_ultima + COALESCE(v_frec_dias, 365) * INTERVAL '1 day')::date;
  v_diff := v_proxima - CURRENT_DATE;

  IF v_diff < 0 THEN
    v_estado := 'VENCIDO';
  ELSIF v_diff < 7 THEN
    v_estado := 'PROXIMO';
  ELSE
    v_estado := 'CUMPLIDO';
  END IF;

  UPDATE "CRONOGRAMA_PREVENTIVO"
     SET "UltimaIntervencion"  = v_ultima,
         "ProximaIntervencion" = v_proxima,
         "Estado"              = v_estado
   WHERE "Id" = NEW."CronogramaId";

  RETURN NEW;
END;
$$;

--------------------------------------------------------------

CREATE FUNCTION public.fn_revert_cronograma_desde_ip() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
    v_frec_dias INTEGER;
    v_ultima    DATE;
    v_proxima   DATE;
    v_estado    TEXT;
    v_diff      INTEGER;
BEGIN
    SELECT "FrecuenciaDias"
      INTO v_frec_dias
    FROM "CRONOGRAMA_PREVENTIVO"
    WHERE "Id" = OLD."CronogramaId";

    v_ultima := OLD."UltimaAntesInsert";

    IF v_ultima IS NULL THEN
        UPDATE "CRONOGRAMA_PREVENTIVO"
           SET "UltimaIntervencion"  = NULL,
               "ProximaIntervencion" = NULL,
               "Estado"              = 'CUMPLIDO'
         WHERE "Id" = OLD."CronogramaId";
    ELSE
        v_proxima := (v_ultima + (v_frec_dias || ' days')::interval)::date;
        v_diff := v_proxima - CURRENT_DATE;

        IF v_diff < 0 THEN
            v_estado := 'VENCIDO';
        ELSIF v_diff < 7 THEN
            v_estado := 'PROXIMO';
        ELSE
            v_estado := 'CUMPLIDO';
        END IF;

        UPDATE "CRONOGRAMA_PREVENTIVO"
           SET "UltimaIntervencion"  = v_ultima,
               "ProximaIntervencion" = v_proxima,
               "Estado"              = v_estado
         WHERE "Id" = OLD."CronogramaId";
    END IF;

    RETURN OLD;
END$$;

/* ============================================================
   FUNCIONES DEL CACHE ORDENADO DEL CRONOGRAMA
   ============================================================ */

CREATE FUNCTION public.refresh_cronograma_cache_row(p_crono_id integer) RETURNS void
LANGUAGE plpgsql AS $$
DECLARE
  r RECORD;
BEGIN
  SELECT
    c."Id" AS id,
    c."CodigoEquipo",
    e."Descripcion" AS "Equipo",
    COALESCE(s."Nombre",'(sin servicio)') AS "Servicio",
    c."DescripcionTarea",
    c."FrecuenciaDias",
    c."UltimaIntervencion",
    c."ProximaIntervencion",
    c."Estado",
    CASE
      WHEN c."Estado" = 'VENCIDO'  THEN 1
      WHEN c."Estado" = 'PROXIMO'  THEN 2
      WHEN c."Estado" = 'CUMPLIDO' THEN 3
      ELSE 4
    END AS "OrdenEstado"
  INTO r
  FROM "CRONOGRAMA_PREVENTIVO" c
  JOIN "EQUIPO" e ON e."CodigoEquipo" = c."CodigoEquipo"
  LEFT JOIN "SERVICIO" s ON s."Id" = e."ServicioId"
  WHERE c."Id" = p_crono_id
    AND c."DescripcionTarea" <> 'INTERVENCIÓN NO IDENTIFICADA (REGISTRO)';

  IF FOUND THEN
    INSERT INTO public."cronograma_ordenado_cache" (
      id, "CodigoEquipo", "Equipo", "Servicio", "DescripcionTarea",
      "FrecuenciaDias", "UltimaIntervencion", "ProximaIntervencion",
      "Estado", "OrdenEstado"
    )
    VALUES (
      r.id, r."CodigoEquipo", r."Equipo", r."Servicio", r."DescripcionTarea",
      r."FrecuenciaDias", r."UltimaIntervencion", r."ProximaIntervencion",
      r."Estado", r."OrdenEstado"
    )
    ON CONFLICT (id) DO UPDATE SET
      "CodigoEquipo"        = EXCLUDED."CodigoEquipo",
      "Equipo"              = EXCLUDED."Equipo",
      "Servicio"            = EXCLUDED."Servicio",
      "DescripcionTarea"    = EXCLUDED."DescripcionTarea",
      "FrecuenciaDias"      = EXCLUDED."FrecuenciaDias",
      "UltimaIntervencion"  = EXCLUDED."UltimaIntervencion",
      "ProximaIntervencion" = EXCLUDED."ProximaIntervencion",
      "Estado"              = EXCLUDED."Estado",
      "OrdenEstado"         = EXCLUDED."OrdenEstado";
  ELSE
    DELETE FROM public."cronograma_ordenado_cache"
    WHERE id = p_crono_id;
  END IF;
END;
$$;

--------------------------------------------------------------

CREATE FUNCTION public.refresh_cronograma_ordenado_cache() RETURNS void
LANGUAGE plpgsql AS $$
BEGIN
  INSERT INTO public."cronograma_ordenado_cache" (
    id, "CodigoEquipo", "Equipo", "Servicio", "DescripcionTarea",
    "FrecuenciaDias", "UltimaIntervencion", "ProximaIntervencion",
    "Estado", "OrdenEstado"
  )
  SELECT
    c."Id",
    c."CodigoEquipo",
    e."Descripcion",
    COALESCE(s."Nombre", '(sin servicio)'),
    c."DescripcionTarea",
    c."FrecuenciaDias",
    c."UltimaIntervencion",
    c."ProximaIntervencion",
    c."Estado",
    CASE
      WHEN c."Estado" = 'VENCIDO'  THEN 1
      WHEN c."Estado" = 'PROXIMO'  THEN 2
      WHEN c."Estado" = 'CUMPLIDO' THEN 3
      ELSE 4
    END
  FROM "CRONOGRAMA_PREVENTIVO" c
  JOIN "EQUIPO" e ON e."CodigoEquipo" = c."CodigoEquipo"
  LEFT JOIN "SERVICIO" s ON s."Id" = e."ServicioId"
  WHERE c."DescripcionTarea" <> 'INTERVENCIÓN NO IDENTIFICADA (REGISTRO)'
  ON CONFLICT (id) DO UPDATE SET
    "CodigoEquipo"        = EXCLUDED."CodigoEquipo",
    "Equipo"              = EXCLUDED."Equipo",
    "Servicio"            = EXCLUDED."Servicio",
    "DescripcionTarea"    = EXCLUDED."DescripcionTarea",
    "FrecuenciaDias"      = EXCLUDED."FrecuenciaDias",
    "UltimaIntervencion"  = EXCLUDED."UltimaIntervencion",
    "ProximaIntervencion" = EXCLUDED."ProximaIntervencion",
    "Estado"              = EXCLUDED."Estado",
    "OrdenEstado"         = EXCLUDED."OrdenEstado";

  DELETE FROM public."cronograma_ordenado_cache" t
  WHERE NOT EXISTS (
    SELECT 1 FROM "CRONOGRAMA_PREVENTIVO" c
    WHERE c."Id" = t.id
      AND c."DescripcionTarea" <> 'INTERVENCIÓN NO IDENTIFICADA (REGISTRO)'
  );
END;
$$;

/* ============================================================
   PROCEDIMIENTO DE ALTA DE INTERVENCIÓN CORRECTIVA
   ============================================================ */

CREATE FUNCTION public.sp_alta_intervencion_correctiva(
  p_codigo_equipo text,
  p_fecha_intervencion date,
  p_tarea text,
  p_horas_desde_reporte integer DEFAULT NULL,
  p_desc_falla text DEFAULT NULL
) RETURNS TABLE(falla_id bigint, intervencion_id bigint)
LANGUAGE plpgsql AS $$
DECLARE
  v_codigo   text;
  v_tarea    text;
  v_horas    integer;
  v_fecha_rep date;
  v_falla_id  bigint;
BEGIN
  v_codigo := UPPER(TRIM(p_codigo_equipo));
  v_tarea  := UPPER(TRIM(COALESCE(p_tarea, '')));

  v_horas := COALESCE(p_horas_desde_reporte, 0);
  IF v_horas < 0 THEN v_horas := 0; END IF;

  v_fecha_rep := (p_fecha_intervencion - make_interval(hours => v_horas))::date;

  SELECT "Id" INTO v_falla_id
  FROM "FALLA"
  WHERE "CodigoEquipo" = v_codigo
    AND "Fecha" = v_fecha_rep
  LIMIT 1;

  IF v_falla_id IS NULL THEN
    INSERT INTO "FALLA" ("CodigoEquipo","Fecha","Descripcion")
    VALUES (v_codigo, v_fecha_rep, COALESCE(UPPER(TRIM(p_desc_falla)), ''))
    RETURNING "Id" INTO v_falla_id;
  END IF;

  INSERT INTO "INTERVENCION_CORRECTIVA"
      ("FallaId","Tarea","FechaReporte","Fecha","HorasDesdeReporte")
  VALUES
      (v_falla_id, v_tarea, v_fecha_rep, p_fecha_intervencion, v_horas)
  RETURNING "Id" INTO intervencion_id;

  falla_id := v_falla_id;
  RETURN;
END;
$$;

/* ============================================================
   TRIGGERS → CACHE, FALLAS, CRONOGRAMA, EQUIPOS
   ============================================================ */

-- Cache (cronograma)

CREATE FUNCTION public.tr_crono_cache_d() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  PERFORM public.refresh_cronograma_cache_row(OLD."Id");
  RETURN OLD;
END;
$$;

--------------------------------------------------------------

CREATE FUNCTION public.tr_crono_cache_iu() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  PERFORM public.refresh_cronograma_cache_row(NEW."Id");
  RETURN NEW;
END;
$$;

--------------------------------------------------------------

CREATE FUNCTION public.tr_equipo_cache_u() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
  rec RECORD;
BEGIN
  IF (TG_OP = 'UPDATE') AND
     (NEW."Descripcion" IS DISTINCT FROM OLD."Descripcion"
       OR NEW."ServicioId" IS DISTINCT FROM OLD."ServicioId")
  THEN
    FOR rec IN
      SELECT c."Id"
      FROM "CRONOGRAMA_PREVENTIVO" c
      WHERE c."CodigoEquipo" = NEW."CodigoEquipo"
    LOOP
      PERFORM public.refresh_cronograma_cache_row(rec."Id");
    END LOOP;
  END IF;
  RETURN NEW;
END;
$$;

--------------------------------------------------------------

CREATE FUNCTION public.tr_servicio_cache_u() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
  rec RECORD;
BEGIN
  IF (TG_OP = 'UPDATE') AND
     (NEW."Nombre" IS DISTINCT FROM OLD."Nombre")
  THEN
    FOR rec IN
      SELECT c."Id"
      FROM "CRONOGRAMA_PREVENTIVO" c
      JOIN "EQUIPO" e ON e."CodigoEquipo" = c."CodigoEquipo"
      WHERE e."ServicioId" = NEW."Id"
    LOOP
      PERFORM public.refresh_cronograma_cache_row(rec."Id");
    END LOOP;
  END IF;
  RETURN NEW;
END;
$$;


