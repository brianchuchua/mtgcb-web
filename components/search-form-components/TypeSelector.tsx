import styled from 'styled-components';
import { CardType } from '../../features/browse/browseSlice';
import { MappedCardTypes } from '../../features/browse/forms/mappers/mapCardTypes';
import AutocompleteWithNegation from '../AutocompleteWithNegation';

interface TypeSelectorProps {
  types: MappedCardTypes[];
  cardTypes: CardType[];
  updateCardTypes: (cardTypes: CardType[]) => void;
}

const TypeSelector: React.FC<TypeSelectorProps> = ({ types, cardTypes, updateCardTypes }) =>
  types ? (
    <StyledTypeSelector>
      <AutocompleteWithNegation
        label="Card Types"
        options={types}
        selectedOptions={cardTypes}
        setSelectedOptionsRemotely={updateCardTypes}
      />
    </StyledTypeSelector>
  ) : null;

const StyledTypeSelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default TypeSelector;
