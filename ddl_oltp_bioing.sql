-- Sistema de Gestión de Equipos Médicos, DDL básico (excluyendo tablas de Directus y triggers)

-- Types
CREATE TYPE public.estado_falla_enum AS ENUM (
    'PENDIENTE',
    'EN PROCESO',
    'CERRADO',
    'RESUELTO'
);

-- Catalog Tables
CREATE TABLE public."SERVICIO" (
    "Id" smallint NOT NULL,
    "Nombre" text NOT NULL,
    "Descripcion" text
);

CREATE TABLE public."UBICACION" (
    "Id" smallint NOT NULL,
    "Lugar" text
);

CREATE TABLE public."RIESGO" (
    "Id" smallint NOT NULL,
    "Tipo" text NOT NULL
);

CREATE TABLE public."RESPONSABLE" (
    "Id" smallint NOT NULL,
    "Nombre" text NOT NULL,
    "Area" text,
    "Tipo" text,
    CONSTRAINT "RESPONSABLE_Tipo_check"
      CHECK ("Tipo" IN ('INGENIERIA CLINICA','TERCERIZADO'))
);

CREATE TABLE public."PROVEEDOR_COMPRA" (
    "Id" smallint NOT NULL,
    "Nombre" text NOT NULL
);

CREATE TABLE public."PROVEEDOR_MANTENIMIENTO" (
    "Id" smallint NOT NULL,
    "Proveedor_Mantenimiento" text NOT NULL
);

-- Equipo
CREATE TABLE public."EQUIPO" (
    "CodigoEquipo" text NOT NULL,
    "Descripcion" text,
    "NumeroDeSerie" text,
    "Estado" text DEFAULT 'OPERATIVO' NOT NULL,
    "FechaAlta" date,
    "FechaBaja" date,
    "Marca" text,
    "Modelo" text,
    "VidaUtil" text,
    "RiesgoId" smallint NOT NULL,
    "UbicacionId" smallint,
    "ServicioId" smallint NOT NULL,
    "Accesorio" varchar(255),
    CONSTRAINT chk_equipo_estado_upper 
        CHECK (upper("Estado") IN ('BAJA','DEPOSITO','OPERATIVO')),
    CONSTRAINT chk_FechaBaja_vs_FechaAlta 
        CHECK ("FechaBaja" IS NULL OR "FechaBaja" >= "FechaAlta")
);

-- Falla
CREATE TABLE public."ESTADO_FALLA" (
    "Id" integer NOT NULL,
    "Nombre" text NOT NULL
);

CREATE TABLE public."FALLA" (
    "Id" bigint NOT NULL,
    "CodigoEquipo" text NOT NULL,
    "Fecha" timestamp NOT NULL,
    "Descripcion" text NOT NULL,
    "FechaCierre" timestamp,
    "Observaciones" text,
    "EstadoId" integer NOT NULL
);

-- Cronograma Preventivo
CREATE TABLE public."CRONOGRAMA_PREVENTIVO" (
    "Id" bigint NOT NULL,
    "CodigoEquipo" text NOT NULL,
    "DescripcionTarea" text NOT NULL,
    "FrecuenciaDias" integer NOT NULL,
    "UltimaIntervencion" date,
    "ProximaIntervencion" date,
    "Estado" text DEFAULT 'CUMPLIDO' NOT NULL,
    CONSTRAINT "CRONOGRAMA_PREVENTIVO_Estado_check"
        CHECK ("Estado" IN ('CUMPLIDO','PROXIMO','VENCIDO')),
    CONSTRAINT "CRONOGRAMA_PREVENTIVO_FrecuenciaDias_check"
        CHECK ("FrecuenciaDias" > 0)
);

-- Intervenciones
CREATE TABLE public."INTERVENCION_PREVENTIVA" (
    "Id" bigint NOT NULL,
    "CronogramaId" bigint NOT NULL,
    "Fecha" date NOT NULL,
    "Observaciones" text,
    "UltimaAntesInsert" date
);

CREATE TABLE public."INTERVENCION_CORRECTIVA" (
    "Id" bigint NOT NULL,
    "FallaId" bigint NOT NULL,
    "Tarea" text NOT NULL,
    "Fecha" timestamp NOT NULL,
    "Resuelto" boolean DEFAULT false,
    "Observaciones" text,
    "PrevEstadoFalla" text,
    "PrevFechaCierre" timestamp
);

-- Responsables puente
CREATE TABLE public."RESPONSABLE_INTERVENCION_PREVENTIVA" (
    "IntervencionId" bigint NOT NULL,
    "ResponsableId" smallint NOT NULL,
    "Id" bigint NOT NULL
);

CREATE TABLE public."RESPONSABLE_INTERVENCION_CORRECTIVA" (
    "IntervencionId" bigint NOT NULL,
    "ResponsableId" smallint NOT NULL,
    "Id" bigint NOT NULL
);

-- Contables
CREATE TABLE public."DESCRIPCION_CONTABLE" (
    "Id" bigint NOT NULL,
    "CodigoEquipo" text NOT NULL,
    "ResponsableCompra" text,
    "NumeroFactura" text,
    "NumeroRemito" text,
    "Valor" numeric(14,2),
    "ProveedorCompraId" integer
);

CREATE TABLE public."DESCRIPCION_CONTABLE_PROV_MANT" (
    "DescripcionContableId" bigint NOT NULL,
    "ProveedorMantId" smallint NOT NULL,
    "Id" bigint NOT NULL
);

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


