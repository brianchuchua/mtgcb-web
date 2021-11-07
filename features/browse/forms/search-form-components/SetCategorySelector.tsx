import { useState } from 'react';
import { useDispatch } from 'react-redux';
import styled from 'styled-components';
import AutocompleteWithNegation from '../../../../components/AutocompleteWithNegation';
import { expansionCategoryOptions, SetCategory, setExpansionCategories } from '../../browseSlice';

const SetCategorySelector: React.FC = () => {
  const dispatch = useDispatch();

  const updateSetCategories = (selectedExpansionCategories: SetCategory[]) => {
    dispatch(setExpansionCategories({ setCategories: selectedExpansionCategories }));
  };

  const [selectedSetCategories, setSelectedSetCategories] = useState([]);

  return (
    <StyledSetCategorySelector>
      <AutocompleteWithNegation
        label="Set Categories"
        options={expansionCategoryOptions}
        selectedOptions={selectedSetCategories}
        setSelectedOptionsLocally={setSelectedSetCategories}
        setSelectedOptionsRemotely={updateSetCategories}
      />
    </StyledSetCategorySelector>
  );
};

const StyledSetCategorySelector = styled.div(() => ({
  paddingLeft: '8px',
  paddingRight: '8px',
  paddingBottom: '10px',
}));

export default SetCategorySelector;
