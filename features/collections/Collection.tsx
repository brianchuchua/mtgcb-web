import { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import { ResponsiveContainer } from '../../components/layout/ResponsiveContainer';
import { useGetAllCardsMetaQuery, useGetAllSetsMetaQuery } from '../../network/services/mtgcbApi';
import { RootState } from '../../redux/rootReducer';
import { useLocalStorage } from '../../util';
import useDebounce, { searchFieldDebounceTimeMs } from '../../util/useDebounce';
import { setFormVisibility } from './collectionSlice';
import { ConnectedCardGallery } from './ConnectedCardGallery';
import { ConnectedCardTable } from './ConnectedCardTable';
import { ConnectedCollectionDetails } from './ConnectedCollectionDetails';
import { ConnectedSetGallery } from './ConnectedSetGallery';
import { ConnectedSetTable } from './ConnectedSetTable';

export const Collection: React.FC<CollectionProps> = ({ userId }) => {
  const {
    searchQuery,
    oracleTextQuery,
    artistQuery,
    cardTypes,
    cardSets,
    cardRarities,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortBy,
    sortByDirection,
    viewSubject,
    viewMode,
    expansionSearchQuery,
    sortExpansionBy,
    sortExpansionByDirection,
    expansionTypes,
    expansionCategories,
  } = useSelector((state: RootState) => state.collection);

  const debouncedSearchQuery = useDebounce(searchQuery, searchFieldDebounceTimeMs);
  const debouncedOracleTextQuery = useDebounce(oracleTextQuery, searchFieldDebounceTimeMs);
  const debouncedArtistQuery = useDebounce(artistQuery, searchFieldDebounceTimeMs);
  const debouncedExpansionSearchQuery = useDebounce(expansionSearchQuery, searchFieldDebounceTimeMs);

  const [skip, setSkip] = useState(0);
  const [first, setFirst] = useLocalStorage('numberOfCardsPerPage', 50);
  const [page, setPage] = useState(1);

  const [expansionsSkip, setExpansionsSkip] = useState(0);
  const [expansionsFirst, setExpansionsFirst] = useLocalStorage('numberOfExpansionsPerPage', 16);
  const [expansionsPage, setExpansionsPage] = useState(1);

  const [previousTotalResults, setPreviousTotalResults] = useState(null);

  const dispatch = useDispatch();

  useEffect(() => {
    dispatch(setFormVisibility({ isFormVisibile: true }));
    return function cleanUpForm() {
      dispatch(setFormVisibility({ isFormVisibile: false }));
    };
  }, []);

  const { data: cardMetaData, isLoading: isCardMetaDataLoading, error: cardMetaError } = useGetAllCardsMetaQuery({
    sortBy,
    name: debouncedSearchQuery,
    oracleTextQuery: debouncedOracleTextQuery,
    artistQuery: debouncedArtistQuery,
    cardSets,
    cardRarities,
    cardTypes,
    cardColors,
    showAllPrintings,
    cardStatSearches,
    sortByDirection,
  });
  const totalResults = cardMetaData?.data?.count;

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

  const { data: allSetsMetaResponse } = useGetAllSetsMetaQuery({
    name: debouncedExpansionSearchQuery,
    sortBy: sortExpansionBy,
    sortByDirection: sortExpansionByDirection,
    setTypes: expansionTypes,
    setCategories: expansionCategories,
  });
  const allSetsMeta = allSetsMetaResponse?.data;
  const totalExpansionsResults = allSetsMeta?.count || 0;

  useEffect(() => {
    if (expansionsSkip > totalExpansionsResults) {
      setExpansionsSkip(0);
      setExpansionsPage(1);
    }
  }, [expansionsSkip, totalExpansionsResults]);

  return (
    <ResponsiveContainer maxWidth="xl">
      <ConnectedCollectionDetails userId={userId} expansionsFirst={expansionsFirst} expansionsSkip={expansionsSkip} />
      <ContentWrapper>
        {viewSubject === 'cards' && viewMode === 'grid' && (
          <ConnectedCardGallery
            first={first}
            skip={skip}
            page={page}
            setSkip={setSkip}
            setFirst={setFirst}
            setPage={setPage}
            userId={userId}
          />
        )}
        {viewSubject === 'cards' && viewMode === 'table' && (
          <ConnectedCardTable
            first={first}
            skip={skip}
            page={page}
            setSkip={setSkip}
            setFirst={setFirst}
            setPage={setPage}
            userId={userId}
          />
        )}
        {viewSubject === 'sets' && viewMode === 'grid' && (
          <ConnectedSetGallery
            expansionsFirst={expansionsFirst}
            expansionsSkip={expansionsSkip}
            expansionsPage={expansionsPage}
            setExpansionsSkip={setExpansionsSkip}
            setExpansionsFirst={setExpansionsFirst}
            setExpansionsPage={setExpansionsPage}
            userId={userId}
          />
        )}
        {viewSubject === 'sets' && viewMode === 'table' && (
          <ConnectedSetTable
            expansionsFirst={expansionsFirst}
            expansionsSkip={expansionsSkip}
            expansionsPage={expansionsPage}
            setExpansionsSkip={setExpansionsSkip}
            setExpansionsFirst={setExpansionsFirst}
            setExpansionsPage={setExpansionsPage}
            userId={userId}
          />
        )}
      </ContentWrapper>
    </ResponsiveContainer>
  );
};

const ContentWrapper = styled.div(({ theme }) => ({
  marginTop: theme.spacing(0),
}));

interface CollectionProps {
  userId: string;
}
