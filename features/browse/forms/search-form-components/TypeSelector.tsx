import { useEffect, useState } from 'react';
import { useDispatch, useSelector } from 'react-redux';
import styled from 'styled-components';
import AutocompleteWithNegation from '../../../../components/AutocompleteWithNegation';
import { getCardTypes } from '../../../../network/features/browse';
import { RootState } from '../../../../redux/rootReducer';
import { CardType, setCardTypes } from '../../browseSlice';
import { mapCardTypes } from '../mappers';

const TypeSelector: React.FC = () => {
  const dispatch = useDispatch();

  const updateCardTypes = (newCardTypes: CardType[]) => {
    dispatch(setCardTypes({ cardTypes: newCardTypes }));
  };

  const [types, setTypes] = useState(null);
  const { cardTypes } = useSelector((state: RootState) => state.browse);

  useEffect(() => {
    async function fetchTypes() {
      const cardTypesResponse = await getCardTypes();
      const allCardTypes = cardTypesResponse?.data?.data?.cardTypes;
      const allCardTypesMapped = mapCardTypes(allCardTypes);

      setTypes(allCardTypesMapped);
    }
    fetchTypes();
  }, []);

  return (
    types && (
      <StyledTypeSelector>
        <AutocompleteWithNegation
          label="Card Types"
          options={types}
          selectedOptions={cardTypes}
          setSelectedOptionsRemotely={updateCardTypes}
        />
      </StyledTypeSelector>
    )
  );
};

const StyledTypeSelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default TypeSelector;
