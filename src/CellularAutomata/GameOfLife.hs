module GOL (
    GOLState,
    GOLCell,
    golTopology,
    golTransition,
    gol
) where

data GOLState = Dead | Alive
type GOLCell = (Int, Int)

golTopology :: ConwayCell -> Set ConwayCell
golTopology (x, y) = Set.fromList
                      $ filter ((/=) (x, y))
                      $ (\dx dy -> (x+dx, y+dy)) <$> [-1..1] <*> [-1..1]

golTransition :: ConwayCell -> ConwayState -> ConwayState
golTransition (x, y) Alive = let liveNgh = Set.size $ Set.filter (==Alive) $ in if elem liveNgh [2,3] then Alive else Dead
golTransition (x, y) Dead  = let liveNgh = Set.size $ Set.filter (==Alive) $ in if    liveNgh == 3    then Alive else Dead

gol :: Cellular ConwayCell ConwayState
gol = Cellular Dead conwayTopology conwayTransition
