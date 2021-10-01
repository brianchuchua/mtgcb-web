import Grid from '@material-ui/core/Grid';
import Slider from '@material-ui/core/Slider';
import Typography from '@material-ui/core/Typography';
import { Dispatch, SetStateAction, useEffect } from 'react';
import { forceCheck as forceImagesToCheckIfTheyShouldLoad } from 'react-lazyload';
import styled from 'styled-components';
import Pagination from '../../components/Pagination';
import SettingsPanel, { SettingGroup } from '../../components/SettingsPanel';
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
  setCardsPerRow?: Dispatch<SetStateAction<number>>;
  setGalleryWidth?: Dispatch<SetStateAction<number>>;
  totalResults: number;
  settingGroups?: SettingGroup[];
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
  settingGroups = [],
}) => {
  const startOfRange = 1 + skip;
  const numberOfCards = cards?.length ?? 0;
  const endOfRange = numberOfCards < first ? skip + numberOfCards : skip + first;

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
          <RightAlignedGrid container item sm={3} alignItems="center" justify="flex-end">
            <Grid item>
              <NumberOfCardsSelect first={first} setFirst={setFirst} />
            </Grid>
            <Grid item>{settingGroups.length > 0 && <SettingsPanel panelId="cardGallerySettings" settingGroups={settingGroups} />}</Grid>
          </RightAlignedGrid>
        </Grid>
        {setGalleryWidth && setCardsPerRow && (
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
                max={9}
                onChange={(e, value: number) => handleCardsPerRowChange(value, setCardsPerRow)}
              />
            </Grid>
          </Grid>
        )}
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
