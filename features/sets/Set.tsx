import Typography from '@material-ui/core/Typography';
import { Skeleton } from '@material-ui/lab';
import { memo, useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import { ResponsiveContainer } from '../../components/layout/ResponsiveContainer';
import { useGetAllCardsMetaQuery, useGetAllCardsQuery, useGetSetBySlugQuery } from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import CardGallery from '../browse/CardGallery';
import CardTable from '../browse/CardTable';
import titleCase from '../browse/util/titleCase';
import { setFormVisibility } from './setSlice';

interface SetProps {
  setSlug: string;
}

export const Set: React.FC<SetProps> = ({ setSlug }) => {
  const {
    searchQuery,
    oracleTextQuery,
    artistQuery,
    cardTypes,
    cardRarities,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortBy,
    sortByDirection,
    viewSubject,
    viewMode,
    priceType,
  } = useSelector((state: RootState) => state.set);

  const dispatch = useDispatch();

  useEffect(() => {
    dispatch(setFormVisibility({ isFormVisibile: true }));
    return function cleanUpForm() {
      dispatch(setFormVisibility({ isFormVisibile: false }));
    };
  }, []);

  const [skip, setSkip] = useState(0);
  const [first, setFirst] = useState(50);
  const [page, setPage] = useState(1);
  const [previousTotalResults, setPreviousTotalResults] = useState(null);

  const { data: setData, isLoading: isSetLoading, isFetching: isSetFetching, error: setError } = useGetSetBySlugQuery({ slug: setSlug });

  const { data: cardData, isLoading: isCardDataLoading, isFetching: isCardDataFetching, error: cardError } = useGetAllCardsQuery({
    first,
    skip,
    sortBy,
    name: searchQuery,
    oracleTextQuery,
    artistQuery,
    cardSets: [
      {
        category: 'Sets',
        value: setData?.data?.sets?.[0]?.id,
        label: setData?.data?.sets?.[0]?.name,
        exclude: false,
      },
    ],
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortByDirection,
  });

  const {
    data: cardMetaData,
    isLoading: isCardMetaDataLoading,
    isFetching: isCardMetaDataFetching,
    error: cardMetaError,
  } = useGetAllCardsMetaQuery({
    sortBy,
    name: searchQuery,
    oracleTextQuery,
    artistQuery,
    cardSets: [
      {
        category: 'Sets',
        value: setData?.data?.sets?.[0]?.id,
        label: setData?.data?.sets?.[0]?.name,
        exclude: false,
      },
    ],
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortByDirection,
  });

  const set = setData?.data?.sets?.[0];
  const cards = cardData?.data?.cards;
  const totalResults = cardMetaData?.data?.count;

  const isLoading = isSetLoading || isCardDataLoading || isCardMetaDataLoading;
  const isFetching = isSetFetching || isCardDataFetching || isCardMetaDataFetching;

  useEffect(() => {
    if (totalResults !== previousTotalResults) {
      setSkip(0);
      setPage(1);
      setPreviousTotalResults(totalResults);
    }
    if (skip > totalResults) {
      setSkip(0);
      setPage(1);
    }
  }, [skip, totalResults, previousTotalResults]);

  // TODO: Make a nice set icon component with intelligent fallbacks or a default option
  // TODO: Add buy links here and come up with a good interface, similar to how Scryfall does card pages perhaps
  // TODO: Add charts/analysis/something cool here

  return (
    <ResponsiveContainer maxWidth="xl">
      <div>
        {set && (
          <div>
            {(isLoading || setSlug !== setData?.data?.sets?.[0].slug) && (
              <CenteredSkeleton variant="rect" width="100%">
                <div style={{ textAlign: 'center' }}>
                  <Typography variant="h4" component="div">
                    {set?.name}
                  </Typography>
                  <i
                    className={`ss ss-${set.code.toLowerCase()} ss-5x ss-common ss-fw`}
                    style={{
                      // WebkitTextStroke: '1px #fff', // TODO: Use this style for a complete set so I can support ss-mythic ss-grad
                      textShadow: '-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff',
                      paddingBottom: '5px',
                    }}
                  />
                  <Typography variant="body2" color="textSecondary" component="div">
                    {set.releasedAt?.slice(0, 10)}
                  </Typography>
                  <Typography variant="body2" color="textSecondary" component="div">
                    {set.cardCount ? `${set.cardCount} cards` : ''}
                  </Typography>
                  <Typography variant="body2" color="textSecondary" component="div">
                    {set.category} Set
                    {set.setType ? ` - ${titleCase(set.setType)}` : ''}
                  </Typography>
                </div>
              </CenteredSkeleton>
            )}
            {!isLoading && (
              <div style={{ textAlign: 'center' }}>
                <Typography variant="h4" component="div">
                  {set?.name}
                </Typography>
                <i
                  className={`ss ss-${set.code.toLowerCase()} ss-5x ss-common ss-fw`}
                  style={{
                    // WebkitTextStroke: '1px #fff', // TODO: Use this style for a complete set so I can support ss-mythic ss-grad
                    textShadow: '-1px -1px 0 #fff, 1px -1px 0 #fff, -1px 1px 0 #fff, 1px 1px 0 #fff',
                    paddingBottom: '5px',
                  }}
                />
                <Typography variant="body2" color="textSecondary" component="div">
                  {set.releasedAt?.slice(0, 10)}
                </Typography>
                <Typography variant="body2" color="textSecondary" component="div">
                  {set.cardCount ? `${set.cardCount} cards` : ''}
                </Typography>
                <Typography variant="body2" color="textSecondary" component="div">
                  {set.category} Set
                  {set.setType ? ` - ${titleCase(set.setType)}` : ''}
                </Typography>
              </div>
            )}
            {viewSubject === 'cards' && viewMode === 'grid' && (
              <MemoizedCardGallery
                cards={cards}
                totalResults={totalResults}
                first={first}
                skip={skip}
                page={page}
                setSkip={setSkip}
                setFirst={setFirst}
                setPage={setPage}
                priceType={priceType}
                isLoading={isLoading}
                isFetching={isFetching}
              />
            )}
            {viewSubject === 'cards' && viewMode === 'table' && (
              <MemoizedCardTable
                cards={cards}
                totalResults={totalResults}
                first={first}
                skip={skip}
                page={page}
                setSkip={setSkip}
                setFirst={setFirst}
                setPage={setPage}
                priceType={priceType}
                isShowingSingleSet
                isFetching={isFetching}
                isLoading={isLoading}
              />
            )}
          </div>
        )}
        {((setData?.data?.sets && setData?.data?.sets.length === 0) || setError) && <p>No set found</p>}
      </div>
    </ResponsiveContainer>
  );
};

const MemoizedCardGallery = memo(CardGallery);
const MemoizedCardTable = memo(CardTable);

const CenteredSkeleton = styled(Skeleton)({
  margin: '0 auto',
});
