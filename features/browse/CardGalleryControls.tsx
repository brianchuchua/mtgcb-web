import { Dispatch, SetStateAction, useEffect } from 'react';
import styled from 'styled-components';
import Grid from '@material-ui/core/Grid';
import Slider from '@material-ui/core/Slider';
import Typography from '@material-ui/core/Typography';
import { forceCheck as forceImagesToCheckIfTheyShouldLoad } from 'react-lazyload';
import Pagination from '../../components/Pagination';
import { NumberOfCardsSelect } from './forms';

interface Card {
  id: number;
  name: string;
  set: {
    name: string;
  };
}

interface CardGalleryControlsProps {
  cards: Card[];
  cardsPerRow?: number;
  page: number;
  first: number;
  skip: number;
  setSkip: Dispatch<SetStateAction<number>>;
  setFirst: Dispatch<SetStateAction<number>>;
  setPage: Dispatch<SetStateAction<number>>;
  setCardsPerRow: Dispatch<SetStateAction<number>>;
  setGalleryWidth: Dispatch<SetStateAction<number>>;
  totalResults: number;
}

const CardGalleryControls: React.FC<CardGalleryControlsProps> = ({
  cards,
  first,
  skip,
  page,
  totalResults,
  setSkip,
  setFirst,
  setPage,
  setCardsPerRow,
  setGalleryWidth,
}) => {
  const startOfRange = 1 + skip;
  const endOfRange = cards.length < first ? skip + cards.length : skip + first;

  const atLeastOneCardToShow = totalResults > 0;

  useEffect(() => {
    forceImagesToCheckIfTheyShouldLoad();
  }, [cards, first, skip, page, totalResults]);

  return (
    atLeastOneCardToShow && (
      <>
        <Grid container spacing={2} justify="space-between" alignItems="center">
          <Grid item sm={3}>
            <Typography>{`Showing ${startOfRange}-${endOfRange} of ${totalResults} cards`}</Typography>
          </Grid>
          <CenteredGrid item sm={6}>
            <Pagination total={totalResults} page={page} first={first} setPage={setPage} setSkip={setSkip} />
          </CenteredGrid>
          <RightAlignedGrid item sm={3}>
            <NumberOfCardsSelect first={first} setFirst={setFirst} />
          </RightAlignedGrid>
        </Grid>
        <Grid container item sm={12} spacing={2} justify="space-between" alignItems="center">
          <Grid item sm={6}>
            <Typography id="card-size-slider">Card size</Typography>
            <Slider
              defaultValue={100}
              aria-labelledby="card-size-slider"
              valueLabelDisplay="auto"
              step={5}
              min={0}
              max={100}
              onChange={(e, value: number) => handleGalleryWidthChange(value, setGalleryWidth)}
            />
          </Grid>
          <Grid item sm={6}>
            <Typography id="cards-per-row-slider">Cards per row</Typography>
            <Slider
              defaultValue={4}
              aria-labelledby="cards-per-row-slider"
              valueLabelDisplay="auto"
              step={1}
              marks
              min={1}
              max={8}
              onChange={(e, value: number) => handleCardsPerRowChange(value, setCardsPerRow)}
            />
          </Grid>
        </Grid>
      </>
    )
  );
};

const handleGalleryWidthChange = (value, setGalleryWidth) => {
  setGalleryWidth(value);
  forceImagesToCheckIfTheyShouldLoad();
};

const handleCardsPerRowChange = (value, setCardsPerRow) => {
  setCardsPerRow(value);
  forceImagesToCheckIfTheyShouldLoad();
};

const CenteredGrid = styled(Grid)({
  textAlign: 'center',
});

const RightAlignedGrid = styled(Grid)({
  textAlign: 'right',
});

export default CardGalleryControls;
