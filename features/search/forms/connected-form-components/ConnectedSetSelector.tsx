import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import AutocompleteWithNegation from '../../../../components/AutocompleteWithNegation';
import { useGetAllSetNamesQuery } from '../../../../network/services/mtgcbApi';
import { RootState } from '../../../../redux/rootReducer';
import { CardSet } from '../../../browse/browseSlice';
import { mapCardSets } from '../../../browse/forms/mappers';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedSetSelectorProps extends ConnectedSearchFormComponentProps {
  setCardSets: any;
}

const ConnectedSetSelector: React.FC<ConnectedSetSelectorProps> = ({ reduxSlice, setCardSets }) => {
  const dispatch = useDispatch();

  const updateSets = (newSets: CardSet[]) => {
    dispatch(setCardSets({ cardSets: newSets }));
  };

  const { cardSets } = useSelector((state: RootState) => state[reduxSlice]);

  const { data: allSetsMetaResponse } = useGetAllSetNamesQuery({});
  const allSetNames = allSetsMetaResponse?.data?.allSets;
  const sets = mapCardSets(allSetNames || []);

  return (
    sets && (
      <StyledSetSelector>
        <AutocompleteWithNegation label="Card Sets" options={sets} selectedOptions={cardSets} setSelectedOptionsRemotely={updateSets} />
      </StyledSetSelector>
    )
  );
};

const StyledSetSelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default ConnectedSetSelector;
