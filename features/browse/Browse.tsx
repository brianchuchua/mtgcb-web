import Breadcrumbs from '@material-ui/core/Breadcrumbs';
import Container from '@material-ui/core/Container';
import { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import Link from '../../components/Link';
import { costToPurchaseAll, getAllCards, getAllCardsMeta, getCardSets, getCardSetsMeta } from '../../network/features/browse';
import { determineSortFilter } from '../../network/features/browse/filters';
import { RootState } from '../../redux/rootReducer';
import { useDebouncedEffect } from '../../util';
import { setFormVisibility } from './browseSlice';
import CardGallery from './CardGallery';
import CardTable from './CardTable';
import SetGallery from './SetGallery';

export const Browse: React.FC = () => {
  const {
    searchQuery,
    oracleTextQuery,
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
    priceType,
    expansionsSearchQuery,
    expansionsSortBy,
    expansionsSortByDirection,
  } = useSelector((state: RootState) => state.browse);
  const [cards, setCards] = useState([]);
  const [totalResults, setTotalResults] = useState(0);
  const [skip, setSkip] = useState(0);
  const [first, setFirst] = useState(50);
  const [page, setPage] = useState(1);
  const [previousQuery, setPreviousQuery] = useState('');
  const [previousOracleTextQuery, setPreviousOracleTextQuery] = useState('');
  const [previousCardTypes, setPreviousCardTypes] = useState([]);
  const [previousCardSets, setPreviousCardSets] = useState([]);
  const [previousCardRarities, setPreviousCardRarities] = useState([]);
  const [previousColors, setPreviousColors] = useState({});
  const [previousShowAllPrintings, setPreviousShowAllPrintings] = useState(true);
  const [previousCardStatSearches, setPreviousCardStatSearches] = useState([]);
  const [previousSortBy, setPreviousSortBy] = useState('releasedAt');
  const [previousSortByDirection, setPreviousSortByDirection] = useState('ASC');
  const [previousSetFirst, setPreviousSetFirst] = useState(50);
  const [previousPriceType, setPreviousPriceType] = useState('market');

  const [expansions, setExpansions] = useState([]);
  const [costsToPurchase, setCostsToPurchase] = useState([]);
  const [totalExpansionsResults, setTotalExpansionsResults] = useState(0);
  const [expansionsSkip, setExpansionsSkip] = useState(0);
  const [expansionsFirst, setExpansionsFirst] = useState(50);
  const [expansionsPage, setExpansionsPage] = useState(1);
  const [expansionsPreviousQuery, setExpansionsPreviousQuery] = useState('');
  const [previousExpansionSortBy, setPreviousExpansionSortBy] = useState('releasedAt');
  const [previousExpansionSortByDirection, setPreviousExpansionSortByDirection] = useState('DESC');
  const [previousExpansionFirst, setPreviousExpansionFirst] = useState(50);
  const dispatch = useDispatch();

  useEffect(() => {
    dispatch(setFormVisibility({ isFormVisibile: true }));
    return function cleanUpForm() {
      dispatch(setFormVisibility({ isFormVisibile: false }));
    };
  }, [dispatch]);

  useDebouncedEffect(
    async () => {
      async function fetchCards() {
        const cardFilterChanged =
          previousQuery !== searchQuery ||
          previousCardTypes !== cardTypes ||
          previousCardSets !== cardSets ||
          previousCardRarities !== cardRarities ||
          previousOracleTextQuery !== oracleTextQuery ||
          previousColors !== cardColors ||
          previousShowAllPrintings !== showAllPrintings ||
          previousCardStatSearches !== cardStatSearches ||
          previousSortBy !== sortBy ||
          previousSortByDirection !== sortByDirection ||
          previousSetFirst !== first ||
          previousPriceType !== priceType;

        const currentPage = cardFilterChanged ? 1 : page;
        setPage(currentPage);
        setSkip((currentPage - 1) * first);

        setPreviousQuery(searchQuery);
        setPreviousOracleTextQuery(oracleTextQuery);
        setPreviousCardTypes(cardTypes);
        setPreviousCardSets(cardSets);
        setPreviousCardRarities(cardRarities);
        setPreviousColors(cardColors);
        setPreviousShowAllPrintings(showAllPrintings);
        setPreviousCardStatSearches(cardStatSearches);
        setPreviousSortBy(sortBy);
        setPreviousSortByDirection(sortByDirection);
        setPreviousSetFirst(first);
        setPreviousPriceType(priceType);

        const allCardsResponse = await getAllCards({
          name: searchQuery,
          oracleTextQuery,
          first,
          skip,
          cardTypes,
          cardSets,
          cardRarities,
          cardColors,
          showAllPrintings,
          cardStatSearches,
          sortBy: determineSortFilter(sortBy, sortByDirection),
        });
        const allCards = allCardsResponse?.data?.data?.allCards;
        setCards(allCards);

        const allCardsMetaResponse = await getAllCardsMeta({
          name: searchQuery,
          oracleTextQuery,
          first,
          skip,
          cardTypes,
          cardSets,
          cardRarities,
          cardColors,
          showAllPrintings,
          cardStatSearches,
          sortBy: determineSortFilter(sortBy, sortByDirection),
        });
        const count = allCardsMetaResponse?.data?.data?._allCardsMeta?.count || 0;
        setTotalResults(count);
      }
      fetchCards();
    },
    400,
    [
      searchQuery,
      oracleTextQuery,
      cardTypes,
      cardSets,
      cardRarities,
      cardColors,
      showAllPrintings,
      cardStatSearches,
      skip,
      first,
      sortBy,
      sortByDirection,
    ]
  );

  useDebouncedEffect(
    async () => {
      async function fetchSets() {
        const setFilterChanged =
          expansionsPreviousQuery !== expansionsSearchQuery ||
          previousExpansionSortBy !== expansionsSortBy ||
          previousExpansionSortByDirection !== expansionsSortByDirection ||
          previousExpansionFirst !== expansionsFirst;

        const currentPage = setFilterChanged ? 1 : expansionsPage;
        setExpansionsPage(currentPage);
        setExpansionsSkip((currentPage - 1) * expansionsFirst);

        setExpansionsPreviousQuery(expansionsSearchQuery);
        setPreviousExpansionSortBy(expansionsSortBy);
        setPreviousExpansionSortByDirection(expansionsSortByDirection);
        setPreviousExpansionFirst(expansionsFirst);

        const allExpansionsResponse = await getCardSets({ first: expansionsFirst, skip: expansionsSkip });
        const allExpansions = allExpansionsResponse?.data?.data?.allSets;
        setExpansions(allExpansions);

        const allExpansionsMetaResponse = await getCardSetsMeta();
        const count = allExpansionsMetaResponse?.data?.data?._allSetsMeta?.count || 0;
        setTotalExpansionsResults(count);

        if (costsToPurchase.length === 0) {
          const costToPurchaseAllResponse = await costToPurchaseAll();
          const costToPurchaseAllData = costToPurchaseAllResponse?.data?.data?.costToPurchaseAll?.costToPurchaseAll;
          setCostsToPurchase(costToPurchaseAllData);
        }
      }
      fetchSets();
    },
    400,
    [expansionsSearchQuery, expansionsSortBy, expansionsSortByDirection, expansionsFirst, expansionsSkip, expansionsPage]
  );

  // TODO: Make better Breadcrumbs component
  return (
    <Container maxWidth="xl">
      <Breadcrumbs separator=">" aria-label="breadcrumb">
        <Link href="/" variant="body2" color="inherit">
          MTG CB
        </Link>
        <Link href="#" variant="body2" color="inherit">
          Browse
        </Link>
      </Breadcrumbs>
      <ContentWrapper>
        {viewSubject === 'cards' && viewMode === 'grid' && (
          <CardGallery
            cards={cards}
            totalResults={totalResults}
            first={first}
            skip={skip}
            page={page}
            setSkip={setSkip}
            setFirst={setFirst}
            setPage={setPage}
            priceType={priceType}
          />
        )}
        {viewSubject === 'cards' && viewMode === 'table' && (
          <CardTable
            cards={cards}
            totalResults={totalResults}
            first={first}
            skip={skip}
            page={page}
            setSkip={setSkip}
            setFirst={setFirst}
            setPage={setPage}
            priceType={priceType}
          />
        )}
        {viewSubject === 'sets' && viewMode === 'grid' && (
          <SetGallery
            sets={expansions}
            costsToPurchase={costsToPurchase}
            totalResults={totalExpansionsResults}
            first={expansionsFirst}
            skip={expansionsSkip}
            page={expansionsPage}
            setSkip={setExpansionsSkip}
            setFirst={setExpansionsFirst}
            setPage={setExpansionsPage}
            priceType={priceType}
          />
        )}
        {viewSubject === 'sets' && viewMode === 'table' && <div>Set Table</div>}
      </ContentWrapper>
    </Container>
  );
};

const ContentWrapper = styled.div(({ theme }) => ({
  marginTop: theme.spacing(0),
}));
