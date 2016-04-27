module Database.Beam.Internal where

class Eq (BeamBackendValue be) => BeamBackend be where
    data BeamBackendValue be :: *
    sqlNull :: BeamBackendValue be
