import { useDispatch, useSelector } from 'react-redux';
import CardEditingModeSelector from '../../../../components/search-form-components/CardEditingModeSelector';
import { RootState } from '../../../../redux/rootReducer';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedCardEditingModeSelectorProps extends ConnectedSearchFormComponentProps {
  setCardEditingMode: any;
}

const ConnectedCardEditingModeSelector: React.FC<ConnectedCardEditingModeSelectorProps> = ({ reduxSlice, setCardEditingMode }) => {
  const dispatch = useDispatch();
  const { cardEditingMode } = useSelector((state: RootState) => state[reduxSlice]);

  const updateCardEditingMode = (cardEditingModeValue) => {
    dispatch(setCardEditingMode(cardEditingModeValue));
  };

  return <CardEditingModeSelector cardEditingMode={cardEditingMode} setCardEditingMode={updateCardEditingMode} />;
};

export default ConnectedCardEditingModeSelector;
