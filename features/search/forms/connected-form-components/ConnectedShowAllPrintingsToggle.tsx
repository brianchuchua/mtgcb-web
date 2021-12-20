import { useDispatch, useSelector } from 'react-redux';
import ShowAllPrintingsToggle from '../../../../components/search-form-components/ShowAllPrintingsToggle';
import { RootState } from '../../../../redux/rootReducer';
import { ConnectedSearchFormComponentProps } from './types';

interface ConnectedShowAllPrintingsToggleProps extends ConnectedSearchFormComponentProps {
  setShowAllPrintings: any;
}

const ConnectedShowAllPrintingsToggle: React.FC<ConnectedShowAllPrintingsToggleProps> = ({ reduxSlice, setShowAllPrintings }) => {
  const dispatch = useDispatch();
  const { showAllPrintings } = useSelector((state: RootState) => state[reduxSlice]);

  const updateShowAllPrintings = (toggleValue) => {
    dispatch(setShowAllPrintings(toggleValue));
  };

  return <ShowAllPrintingsToggle showAllPrintings={showAllPrintings} updateShowAllPrintings={updateShowAllPrintings} />;
};

export default ConnectedShowAllPrintingsToggle;
